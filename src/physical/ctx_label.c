/****************************************************************
 *								*
 * Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

#include "physical_plan.h"
#include "template_helpers.h"

/* YDBOcto#1146 : Registry of the context labels ("octoExtractNN" for an EXTRACT column expression, "octoIteratorNN"
 * for an ITERATOR key column expression) that a generated plan wraps around each such expression that can call M
 * code (see "ctx_label_needed()"; nothing else can read the context).
 *
 * Both expressions are emitted in the middle of a larger M expression -- an EXTRACT inside the SELECT column list,
 * an ITERATOR inside the "FOR  SET <key>=..." of the key loop -- so there is no statement boundary at which the
 * table/column context could be set and later restored. An extrinsic that NEWs "%ydboctoctx" is the only M
 * construct that restores on exit (including on error unwind), so each expression that needs one is cut out of the
 * plan buffer as it is emitted, stashed here, and replaced in place by a "$$octoExtractNN()" (or
 * "$$octoIteratorNN()") call. The stashed bodies are then emitted as labels at the bottom of the same
 * routine.
 *
 * See "ctx_label_stash()" (invoked from the *.ctemplate emission paths) and "ctx_label_emit()" (invoked from
 * "emit_physical_plan()" and "emit_xref_plan()" once all "octoPlanNN"/"xrefPlan" labels have been written).
 */

/* One context label. Private to this file: nothing outside it has any business walking the list. */
typedef struct CtxLabel {
	CtxLabelType	 type;	      /* Which keyword this label wraps */
	char		*table_name;  /* Table owning the column. First $CHAR(0) piece of "%ydboctoctx". */
	char		*column_name; /* The column itself. Second $CHAR(0) piece of "%ydboctoctx". */
	char		*body;	      /* The keyword expression, cut out of the plan emission buffer. Not NUL terminated. */
	uint64_t	 body_len;    /* Number of bytes in "body" */
	int		 label_num;   /* The NN in "octoExtractNN"/"octoIteratorNN". Numbered separately per "type". */
	struct CtxLabel *next;
} CtxLabel;

/* Head of the labels accumulated so far for the M routine currently being emitted. This is per-routine state, not
 * per-plan state: one routine holds several "octoPlanNN" labels and the numbering has to be unique across all of
 * them, so no individual "PhysicalPlan" can own it. "ctx_label_reset()" starts a routine and "ctx_label_emit()"
 * ends one by writing the labels out and emptying the list again.
 */
static CtxLabel *ctx_label_list;

/* Returns the M label name prefix used for labels of the given "type". */
static char *ctx_label_prefix(CtxLabelType type) {
	return ((CtxLabelType_Iterator == type) ? OCTO_ITERATOR_LIT : OCTO_EXTRACT_LIT);
}

/* Returns the SQL keyword name that labels of the given "type" wrap. Used only in the emitted comment. */
static char *ctx_label_keyword(CtxLabelType type) { return ((CtxLabelType_Iterator == type) ? "ITERATOR" : "EXTRACT"); }

/* Returns the number of labels of type "type" currently registered. Also sets "*last" to the tail of the list
 * (NULL if empty) so callers that need to append do not have to walk the list a second time. Labels are numbered
 * separately per type, since each type has a name prefix of its own.
 */
static int ctx_label_count(CtxLabelType type, CtxLabel **last) {
	CtxLabel *iterator_label;
	int	  num_labels;

	num_labels = 0;
	*last = NULL;
	for (iterator_label = ctx_label_list; NULL != iterator_label; iterator_label = iterator_label->next) {
		if (type == iterator_label->type) {
			num_labels++;
		}
		*last = iterator_label;
	}

	return num_labels;
}

/* Starts a new M routine with no labels registered. */
void ctx_label_reset(void) { ctx_label_list = NULL; }

#ifndef NDEBUG
/* Lets callers assert the "one routine at a time" invariant without exposing the list itself. */
boolean_t ctx_label_list_is_empty(void) { return (NULL == ctx_label_list); }
#endif

/* Returns TRUE if "body" (an already emitted EXTRACT or ITERATOR expression of "body_len" bytes) directly calls an M
 * function, i.e. contains an extrinsic ("$$"), an external call ("$&") or indirection ("@"). Only such a call can read
 * the context, so an expression with none of those is left inline and pays nothing for it. A match inside a string
 * literal costs an unnecessary label, never a wrong answer.
 *
 * M code reached indirectly is deliberately not covered. A "$INCREMENT(^GBL)" in the expression can fire a trigger on
 * "^GBL", and that trigger's M code sees no context. Covering it would mean wrapping every expression that updates a
 * global, which puts the cost back on expressions that never read the context.
 */
boolean_t ctx_label_needed(char *body, uint64_t body_len) {
	uint64_t index;

	for (index = 0; index < body_len; index++) {
		if ('@' == body[index]) {
			return TRUE; /* Indirection can evaluate an extrinsic */
		}
		if (('$' == body[index]) && ((index + 1) < body_len) && (('$' == body[index + 1]) || ('&' == body[index + 1]))) {
			return TRUE; /* Extrinsic function or external call */
		}
	}
	return FALSE; /* The expression calls no M function directly, so nothing that can observe the context runs */
}

/* Registers "body" (an already emitted EXTRACT or ITERATOR expression of "body_len" bytes, cut out of the plan
 * emission buffer) as the body of a label computing "column_name" of "table_name". Returns the NN to use in the
 * "$$octoExtractNN()"/"$$octoIteratorNN()" call that replaces the cut expression.
 */
int ctx_label_stash(CtxLabelType type, char *table_name, char *column_name, char *body, uint64_t body_len) {
	CtxLabel *iterator_label, *last_label, *new_label;
	int	  num_labels;

	/* De-duplicate on the table, the column AND the emitted body, so that a label is shared exactly when the two
	 * references are interchangeable. TC089 Section C covers the column and body parts, TITER15 Section C the table
	 * part:
	 *	- The body alone is not enough. Two different columns can carry the same EXTRACT text (e.g. two columns
	 *	  both defined as EXTRACT "$$F^R(keys(""id""))"), which emits identical M; sharing a label between them
	 *	  would report the first column's name for both.
	 *	- The table/column pair alone is not enough either. One column emits different M in different places,
	 *	  since a "keys(...)" reference inside it carries the unique id of the table alias it was reached
	 *	  through: "SELECT a.col, b.col FROM t a, t b" emits two different bodies for the one column. That holds
	 *	  for a column reached through another EXTRACT column's "values(...)" too: it inherits the outer column's
	 *	  alias, so "SELECT a.outer, b.outer" gets two "inner" labels, and must.
	 *	- The type need not be compared: table and column already determine it, since a column can never carry
	 *	  both keywords (ITERATOR requires a key column, EXTRACT forbids one). An assert checks that.
	 */
	for (iterator_label = ctx_label_list; NULL != iterator_label; iterator_label = iterator_label->next) {
		if ((body_len != iterator_label->body_len) || (0 != memcmp(body, iterator_label->body, body_len))) {
			continue;
		}
		if ((0 == strcmp(table_name, iterator_label->table_name))
		    && (0 == strcmp(column_name, iterator_label->column_name))) {
			assert(type == iterator_label->type); /* table + column determine the type (see comment above) */
			return iterator_label->label_num;
		}
	}

	num_labels = ctx_label_count(type, &last_label);

	/* Both allocations below come from the query's memory chunk, so neither is freed here: the whole chunk is
	 * released by the "OCTO_CFREE(memory_chunks)" that ends the query in "run_query.c". This is the same lifetime
	 * "octoLeftJoinNN" gives its cut-out body in "tmpl_tablejoin.ctemplate", which also "octo_cmalloc"s and never
	 * frees. "ctx_label_emit()" only drops the list head; it does not own the memory.
	 */
	new_label = (CtxLabel *)octo_cmalloc(memory_chunks, sizeof(CtxLabel));
	new_label->type = type;
	new_label->table_name = table_name;
	new_label->column_name = column_name;
	new_label->body = (char *)octo_cmalloc(memory_chunks, body_len);
	memcpy(new_label->body, body, body_len);
	new_label->body_len = body_len;
	new_label->label_num = num_labels + 1;
	new_label->next = NULL;

	if (NULL == last_label) {
		ctx_label_list = new_label;
	} else {
		last_label->next = new_label;
	}

	return new_label->label_num;
}

/* Emits every registered context label into "memstream". Caller is expected to have finished emitting all
 * "octoPlanNN" (or "xrefPlan") labels of the routine first, since the labels are appended at the bottom of it.
 */
void ctx_label_emit(FILE *memstream) {
	CtxLabel *iterator_label;
	char	 *escaped_buffer;
	int	  escaped_buffer_size;

	if (NULL == ctx_label_list) {
		return; /* No EXTRACT or ITERATOR expression was emitted by this routine, so there is no label to emit */
	}

	escaped_buffer_size = OCTO_INIT_BUFFER_LEN;
	escaped_buffer = (char *)malloc(sizeof(char) * escaped_buffer_size);

	for (iterator_label = ctx_label_list; NULL != iterator_label; iterator_label = iterator_label->next) {
		/* The label takes no parameters. Its body reads "cursorId" from the caller's frame through M's dynamic
		 * scoping, exactly as "octoLeftJoinNN" does, and every caller has it bound: an "octoPlanNN(cursorId)" body,
		 * an "octoLeftJoinNN" body, an xref routine's "xrefPlan(cursorId)", or an enclosing context label.
		 */
		fprintf(memstream, "\n%s%d()\t; %s context for %s.%s\n", ctx_label_prefix(iterator_label->type),
			iterator_label->label_num, ctx_label_keyword(iterator_label->type), iterator_label->table_name,
			iterator_label->column_name);
		fprintf(memstream, "%sNEW %s\n", PLAN_LINE_START, PP_YDB_OCTO_CTX);

		/* One unsubscripted variable holding both names, separated by $CHAR(0) (which no SQL identifier can
		 * contain). Two subscripted nodes cost ~50 ns more per evaluation, because the NEW above has just emptied
		 * the variable and both nodes have to be created afresh every time.
		 */
		m_escape_string2(&escaped_buffer, &escaped_buffer_size, iterator_label->table_name);
		fprintf(memstream, "%sSET %s=\"%s\"_$CHAR(0)_", PLAN_LINE_START, PP_YDB_OCTO_CTX, escaped_buffer);

		m_escape_string2(&escaped_buffer, &escaped_buffer_size, iterator_label->column_name);
		fprintf(memstream, "\"%s\"\n", escaped_buffer);

		fprintf(memstream, "%sQUIT %.*s\n", PLAN_LINE_START, (int)iterator_label->body_len, iterator_label->body);
	}

	free(escaped_buffer);
	ctx_label_list = NULL; /* This routine is done; the next one starts empty */
}
