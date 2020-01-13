/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#ifndef OCTO_H
#define OCTO_H

#include <stdio.h>
#include <limits.h>	/* needed for PATH_MAX */
#include <sys/param.h>	/* needed for PATH_MAX */
#include <stdint.h>	/* needed for uint64_t */

#include <libyottadb.h>

#include "errors.h"
#include "octo_types.h"
#include "config.h"
#include "constants.h"
#include "memory_chunk.h"

#include "mmrhash.h"

// Maximum number of chars needed to convert unsigned int to char*, including null terminator
#define UINT_TO_STRING_MAX 11
// Maximum number of chars needed to convert unsigned long long to char*, including null terminator
#define ULONG_TO_STRING_MAX 21

/* Set OCTO_PATH_MAX to be the same as the system PATH_MAX (should be defined by limits.h or sys/param.h)
 * but in case it is not available, set it to a value of 1024 just like is done in YDB.
 */
#ifdef PATH_MAX
# define OCTO_PATH_MAX 1024
#else
# define OCTO_PATH_MAX PATH_MAX
#endif

// Allows us to leave parameters in place even if they are unused and avoid warning from the
// compiler.
#define UNUSED(x) (void)(x)

// Set maximum M routine length - must be in sync with MAX_MIDENT_LEN in YDB/sr_port/mdef.h
#define MAX_ROUTINE_LEN 31

/* Set maximum trigger name length
 * This cannot be greater than MAX_USER_TRIGNAME_LEN defined in https://gitlab.com/YottaDB/DB/YDB/blob/master/sr_unix/trigger.h
 */
#define MAX_TRIGGER_LEN 28

// Specify character for delimiting column values in Octo
#define COLUMN_DELIMITER "|"

// Default buffer allocated for $zroutines
#define ZRO_INIT_ALLOC 512

// Below are a few utility macros that are similar to those defined in sr_port/gtm_common_defs.h.
// But we do not use those as that is outside the control of Octo's source code repository
// Therefore we undef those macros (if any defined) and redefine it ourselves here.

#undef LIT_LEN			// in case it gets defined in sr_port/gtm_common_defs.h (which is included by mmrhash.h above)
#undef MEMCMP_LIT		// would have been defined by sr_port/gtm_common_defs.h (which is included by mmrhash.h above)
#undef MEMCPY_LIT		// would have been defined by sr_port/gtm_common_defs.h (which is included by mmrhash.h above)

// Returns the length of a string literal (similar to STR_LIT_LEN in sr_port/gtm_common_defs.h)
#define	LIT_LEN(LITERAL)	(sizeof(LITERAL) - 1)

// Do a memcmp of a source string against a string literal
#define	MEMCMP_LIT(SOURCE, LITERAL)		memcmp(SOURCE, LITERAL, sizeof(LITERAL) - 1)

// Do a memcpy onto a string target using a string literal as the source
#define	MEMCPY_LIT(TARGET, LITERAL)		memcpy(TARGET, LITERAL, sizeof(LITERAL) - 1)

#define GET_FULL_PATH_OF_GENERATED_M_FILE(FILENAME, ROUTINE_NAME)						\
{														\
	unsigned int	want_to_write;										\
	int		len;											\
	const char		*tmp_dir;									\
														\
	tmp_dir = config->tmp_dir;										\
	assert(NULL != tmp_dir);										\
	len = strlen(tmp_dir);											\
	assert(len);												\
	if ('/' == tmp_dir[len - 1]) {										\
		/* tmp_dir already has a trailing '/' so no need to add a '/' before the file name */		\
		want_to_write = snprintf(FILENAME, sizeof(FILENAME), "%s_%s.m", tmp_dir, ROUTINE_NAME);		\
	} else {												\
		/* tmp_dir does not have a trailing '/' so need to add a '/' before the file name */		\
		want_to_write = snprintf(FILENAME, sizeof(FILENAME), "%s/_%s.m", tmp_dir, ROUTINE_NAME);	\
	}													\
	if (want_to_write >= (int)sizeof(FILENAME)) {								\
		FATAL(ERR_BUFFER_TOO_SMALL, "");								\
	}													\
}

#define	INVOKE_HASH_CANONICAL_QUERY(STATE, RESULT, STATUS)	\
{								\
	STATUS = 0;						\
	HASH128_STATE_INIT(STATE, 0);				\
	hash_canonical_query_cycle++;				\
	hash_canonical_query(&STATE, RESULT, &STATUS);		\
}

#define LOG_LOCAL_ONLY(SEVERITY, ERROR, ...)	\
{						\
	rocto_session.sending_message = TRUE;	\
	SEVERITY(ERROR, ## __VA_ARGS__);	\
	rocto_session.sending_message = FALSE;	\
}

#define INVOKE_ROW_VALUE_CONSTRUCTOR_BINARY_STATEMENT(RESULT, ROW_VALUE_CONSTRUCTOR, CURSORID)	\
{												\
	RESULT = row_value_constructor_binary_statement(ROW_VALUE_CONSTRUCTOR, CURSORID);	\
	if (NULL == RESULT)									\
		YYABORT;									\
}

#define INVOKE_REGEX_SPECIFICATION(STMT, OP0, OP1, IS_REGEX_LIKE_OR_SIMILAR, IS_SENSITIVE, IS_NOT, CURSORID)		\
{															\
	int status;													\
	status = regex_specification(STMT, OP0, OP1, IS_REGEX_LIKE_OR_SIMILAR, IS_SENSITIVE, IS_NOT, CURSORID);		\
	if (0 != status)												\
		YYABORT;												\
}

#define INVOKE_PARSE_LITERAL_TO_PARAMETER(CURSORID, VALUE_STMT, UPDATE_EXISTING)	\
{											\
	int status;									\
	status = parse_literal_to_parameter(CURSORID, VALUE_STMT, UPDATE_EXISTING);	\
	if (0 != status)								\
		YYABORT;								\
}

/* Below are special values used as part of the `aggregate_depth` parameter passed to `qualify_statement()`
 * to indicate we are qualifying a FROM clause, WHERE clause, GROUP BY clause etc.. These need to be negative as 0 is the
 * first valid depth used in the normal case (e.g. when qualifying the SELECT column list i.e. select->select_list) etc.
 * Some of these negative values indicate clauses where Aggregate functions are disallowed (i.e. FROM and WHERE).
 */
#define	AGGREGATE_DEPTH_FROM_CLAUSE		-1
#define	AGGREGATE_DEPTH_WHERE_CLAUSE		-2
#define	AGGREGATE_DEPTH_GROUP_BY_CLAUSE		-3

int emit_column_specification(char *buffer, int buffer_size, SqlColumn *column);
int emit_create_table(FILE *output, struct SqlStatement *stmt);
// Recursively copies all of stmt, including making copies of strings

/**
 * Examines the table to make sure needed columns are specified, and fills out
 * any that are needed but not present.
 *
 * @returns 0 if success, 1 otherwise
 */
int create_table_defaults(SqlStatement *table_statement, SqlStatement *keywords_statement);

char *m_escape_string(const char *string);
int m_escape_string2(char *buffer, int buffer_len, char *string);
char *m_unescape_string(const char *string);

int readline_get_more();
SqlStatement *parse_line(char *cursorId);

int populate_data_type(SqlStatement *v, SqlValueType *type, char *cursorId);
SqlTable *find_table(const char *table_name);
SqlColumn *find_column(char *column_name, SqlTable *table);
SqlStatement *find_column_alias_name(SqlStatement *stmt);
void init_parent_table_alias(SqlStatement *table_alias_stmt, SqlTableAlias *parent_table_alias);
int qualify_query(SqlStatement *table_alias_stmt, SqlJoin *parent_join, SqlTableAlias *parent_table_alias);
int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *table_alias_stmt,
							int depth, SqlColumnListAlias **ret_cla);
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *table_alias_stmt,
									int depth, SqlColumnListAlias **ret_cla);
SqlColumnListAlias *match_column_in_table(SqlTableAlias *table, char *column_name, int column_name_len, boolean_t *ambiguous);
int qualify_function_name(SqlStatement *stmt);
void print_yyloc(YYLTYPE *llocp);
SqlOptionalKeyword *get_keyword(SqlColumn *column, enum OptionalKeyword keyword);
SqlOptionalKeyword *get_keyword_from_keywords(SqlOptionalKeyword *start_keyword, enum OptionalKeyword keyword);
int get_key_columns(SqlTable *table, SqlColumn **key_columns);
int generate_key_name(char *buffer, int buffer_size, int target_key_num, SqlTable *table, SqlColumn **key_columns);

char *like_to_regex(const char *src);
char *similar_to_regex(const char *src);

char *get_aggregate_func_name(SqlAggregateType type);
char *get_set_operation_string(SqlSetOperationType type);
char *get_user_visible_type_string(SqlValueType type);

/* Hashing support functions */
int generate_routine_name(hash128_state_t *state, char *routine_name, int routine_len, FileType file_type);
void hash_canonical_query(hash128_state_t *state, SqlStatement *stmt, int *status);
void ydb_hash_to_string(ydb_uint16 *hash, char *buffer, const unsigned int buf_len);

void assign_table_to_columns(SqlStatement *table_statement);
SqlOptionalKeyword *add_optional_piece_keyword_to_sql_column(int column_number);

// Converts a list of columns to a column list alias associated with the given table alias
SqlColumnListAlias *columns_to_column_list_alias(SqlColumn *column, SqlStatement *table_alias_stmt);

SqlStatement *drill_to_table_alias(SqlStatement *sqlStmt);
int get_column_piece_number(SqlColumnAlias *column_alias, SqlTableAlias *table_alias);
int get_column_number_from_column_list_alias(SqlColumnListAlias *input_cla, SqlTableAlias *table_alias);
SqlColumnListAlias *get_column_list_alias_n_from_table_alias(SqlTableAlias *table_alias, int column_number);
SqlColumnAlias *get_column_alias_for_column_list_alias(SqlColumnListAlias *col_cla, SqlStatement *matching_alias_stmt);

SqlStatement *copy_sql_statement(SqlStatement *stmt);

void compress_statement(SqlStatement *stmt, char **out, int *out_length);
SqlStatement *decompress_statement(char *buffer, int out_length);
int store_table_in_pg_class(SqlTable *table);
void cleanup_tables();

/* Parse related functions invoked from the .y files (parser.y, select.y etc.) */
SqlStatement *query_specification(OptionalKeyword set_quantifier, SqlStatement *select_list,
					SqlStatement *table_expression, SqlStatement *sort_specification_list, int *plan_id);
SqlStatement *sort_specification(SqlStatement *sort_key, SqlStatement *collate_clause, SqlStatement *ordering_specification);
SqlStatement *grouping_column_reference(SqlStatement *derived_column_expression, SqlStatement *collate_clause);
int regex_specification(SqlStatement **stmt, SqlStatement *op0, SqlStatement *op1, int is_regex_like_or_similar,
									int is_sensitive, int is_not, char *cursorId);
SqlStatement *set_operation(enum SqlSetOperationType setoper_type, SqlStatement *left_operand, SqlStatement *right_operand);
SqlStatement *row_value_constructor_binary_statement(SqlStatement *row_value_constructor, char *cursorId);
SqlStatement *between_predicate(SqlStatement *row_value_constructor, SqlStatement *from, SqlStatement *to, boolean_t not_specified);
SqlStatement *aggregate_function(SqlAggregateType aggregate_type, OptionalKeyword set_quantifier, SqlStatement *value_expression);
SqlStatement *natural_join_condition(SqlStatement *left, SqlStatement *right, boolean_t *ambiguous);

int parse_literal_to_parameter(char *cursorId, SqlValue *value, boolean_t update_existing);

/* trims duplicate '.*'s from regex */
void trim_dot_star(SqlValue *regex);

/**
 * Returns TRUE if the columns are equal, FALSE otherwise
 */
int columns_equal(SqlColumn *a, SqlColumn *b);
int tables_equal(SqlTable *a, SqlTable *b);
int values_equal(SqlValue *a, SqlValue *b);

int no_more();

/* Globals */
SqlTable	*definedTables;
uint64_t	hash_canonical_query_cycle;	// incremented before every outermost call to "hash_canonical_query"
int		cur_input_index;		// Current index of input_buffer_combined the parser should read from, and readlines should write to. Effectively marks the end of the current query.
int		old_input_index;		// The previous value of cur_input_index before the parser modifies it. Effectively marks the start of the current query.
int		leading_spaces;			// leading spaces in the current query it needs to be stored somewhere accessible but should be ignored, except by the lexer and yyerror
int		cur_input_max;
int		cancel_received;
int		eof_hit;
FILE		*inputFile;
FILE		*err_buffer;
char		*input_buffer_combined;		// The input buffer for octo. Contains the query strings.
int		(*cur_input_more)();

int get_input(char *buf, int size);
void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, char *cursorId, char const *s);
#endif
