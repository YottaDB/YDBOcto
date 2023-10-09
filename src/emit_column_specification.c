/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

/* Returns
 *	> 0 if something was emitted for this column (most common case).
 *	  0 if this column was skipped (e.g. hidden key column)
 *	 -1 if there was an error
 */
int emit_column_specification(char **buffer, int *buffer_size, SqlColumn *cur_column) {
	SqlValue *	    value;
	SqlOptionalKeyword *cur_keyword, *start_keyword;
	char		    ch, *delim;
	char **		    buff_ptr, *bufp;
	char *		    buffer2;
	int		    buffer2_size;
	char		    data_type_string[MAX_USER_VISIBLE_TYPE_STRING_LEN];

	DEBUG_ONLY(boolean_t piece_seen = FALSE);
	DEBUG_ONLY(boolean_t empty_delim_seen = FALSE);

	bufp = *buffer;
	buff_ptr = &bufp;
	if (cur_column->is_hidden_keycol) {
		/* This is a hidden key column. Do not emit it in the text table definition as it can confuse the user.
		 * Only emit user specified columns in the text definition.
		 */
		return 0;
	}
	if (NULL != cur_column->columnName) {
		/* Column name is NOT NULL. This means it is a real column in the table (not a table-level constraint) */
		UNPACK_SQL_STATEMENT(value, cur_column->columnName, value);
		if (value->is_double_quoted) {
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "\"%s\" ", value->v.reference);
		} else {
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "`%s` ", value->v.reference);
		}

		int ret;
		ret = get_user_visible_data_type_string(&cur_column->data_type_struct, data_type_string, sizeof(data_type_string));
		if (0 > ret) {
			assert(FALSE);
			return -1;
		}
		INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, "%s", data_type_string);
	}
	UNPACK_SQL_STATEMENT(start_keyword, cur_column->keywords, keyword);
	cur_keyword = start_keyword;
	buffer2_size = OCTO_INIT_BUFFER_LEN;
	buffer2 = (char *)malloc(sizeof(char) * buffer2_size);
	do {
		switch (cur_keyword->keyword) {
		case NOT_NULL:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " NOT NULL");
			break;
		case OPTIONAL_EXTRACT:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " EXTRACT \"%s\"", buffer2);
			break;
		case OPTIONAL_PIECE:
			DEBUG_ONLY(assert(!empty_delim_seen));
			DEBUG_ONLY(piece_seen = TRUE);
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " PIECE %s", buffer2);
			break;
		case OPTIONAL_GLOBAL:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " GLOBAL \"%s\"", buffer2);
			break;
		case OPTIONAL_DELIM:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			delim = value->v.reference;
			ch = *delim;
			delim++; /* Skip first byte to get actual delimiter */
			assert((DELIM_IS_DOLLAR_CHAR == ch) || (DELIM_IS_LITERAL == ch));
			if (DELIM_IS_LITERAL == ch) {
				DEBUG_ONLY(empty_delim_seen = ('\0' == *delim));
				DEBUG_ONLY(assert(!empty_delim_seen || !piece_seen));
				m_escape_string2(&buffer2, &buffer2_size, delim);
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " DELIM \"%s\"",
									    buffer2);
			} else {
				assert(!MEMCMP_LIT(delim, "$CHAR(")); /* this is added in parser.y */
				delim += sizeof("$CHAR") - 1;	      /* Skip "$CHAR" */
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " DELIM %s", delim);
			}
			break;
		case OPTIONAL_KEY_NUM:
			/* KEY NUM is an internal representation for the PRIMARY KEY constraint.
			 * For example 2 columns with KEY NUM 0 and KEY NUM 1 each would translate to a 2-column PRIMARY KEY
			 * constraint. So do not display the KEY NUM keyword. Only display the PRIMARY KEY constraint when
			 * it is encountered.
			 */
			break;
		case NO_KEYWORD:
			break;
		case OPTIONAL_START:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " START \"%s\"", buffer2);
			break;
		case OPTIONAL_STARTINCLUDE:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " STARTINCLUDE");
			break;
		case OPTIONAL_END:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " END \"%s\"", buffer2);
			break;
		case OPTIONAL_ENDPOINT:
			UNPACK_SQL_STATEMENT(value, cur_keyword->v, value);
			m_escape_string2(&buffer2, &buffer2_size, value->v.reference);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " ENDPOINT \"%s\"", buffer2);
			break;
		case OPTIONAL_GENERATED_ALWAYS_IDENTITY:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " GENERATED ALWAYS AS IDENTITY");
			break;
		case OPTIONAL_GENERATED_BY_DEFAULT_IDENTITY:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr,
								    " GENERATED BY DEFAULT AS IDENTITY");
			break;
		case OPTIONAL_MAYBE_CANONICAL:
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " MAYBE_CANONICAL");
			break;
		case PRIMARY_KEY:
		case OPTIONAL_CHECK_CONSTRAINT:
		case UNIQUE_CONSTRAINT:;
			int	       status;
			SqlConstraint *constraint;
			SqlValue *     value;

			UNPACK_SQL_STATEMENT(constraint, cur_keyword->v, constraint);
			assert(cur_keyword->keyword == constraint->type);
			assert(NULL != constraint->name);
			UNPACK_SQL_STATEMENT(value, constraint->name, value);

			assert(value->is_double_quoted);
			INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " CONSTRAINT \"%s\" %s",
								    value->v.string_literal,
								    ((UNIQUE_CONSTRAINT == cur_keyword->keyword) ? "UNIQUE"
								     : (PRIMARY_KEY == cur_keyword->keyword)	 ? "PRIMARY KEY"
														 : "CHECK ("));
			/* Note: "emit_check_constraint()" does the needed emitting not just for a CHECK constraint but
			 * also for a UNIQUE or PRIMARY KEY constraint. That said, we want to emit the list of columns
			 * for the UNIQUE or PRIMARY KEY constraint only if there is more than one column in the list
			 * as otherwise it would lead to a parse error if the generated CREATE TABLE command is used to
			 * recreate the table.
			 */
			boolean_t do_emit_check_constraint;
			switch (cur_keyword->keyword) {
			case PRIMARY_KEY:
			case UNIQUE_CONSTRAINT:;
				SqlStatement *column_name_list;
				column_name_list = constraint->definition;

				SqlColumnList *start_cl;
				UNPACK_SQL_STATEMENT(start_cl, column_name_list, column_list);
				do_emit_check_constraint = (start_cl->next != start_cl);
				if (do_emit_check_constraint) {
					INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, " ");
				}
				break;
			default:
				assert(OPTIONAL_CHECK_CONSTRAINT == cur_keyword->keyword);
				do_emit_check_constraint = TRUE;
				break;
			}
			if (do_emit_check_constraint) {
				status = emit_check_constraint(buffer, buffer_size, buff_ptr, constraint->definition);
				if (0 > status) {
					free(buffer2);
					return -1;
				}
			}
			if (OPTIONAL_CHECK_CONSTRAINT == cur_keyword->keyword) {
				INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(buffer, buffer_size, buff_ptr, ")");
			}
			/* Note that "constraint->v.check_columns" and "constraint->v.uniq_gblname" is information derived
			 * from "constraint->definition" and so is not stored in the text table definition. Hence no processing
			 * for that done here.
			 */
			break;
		default:
			ERROR(ERR_UNKNOWN_KEYWORD_STATE, "");
			assert(FALSE);
			free(buffer2);
			return -1;
			break;
		}
		cur_keyword = cur_keyword->next;
	} while (cur_keyword != start_keyword);
	assert(*buff_ptr - *buffer < *buffer_size);
	**buff_ptr = '\0';
	// We don't count the null character we added for ease of use
	free(buffer2);
	return *buff_ptr - *buffer;
}
