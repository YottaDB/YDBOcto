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

#include "rocto/message_formats.h"
#include "errors.h"
#include "octo_types.h"
#include "config.h"
#include "constants.h"
#include "memory_chunk.h"

#include "mmrhash.h"

// Max number of chars needed to convert signed or unsigned int to char*, including null terminator and possible negative sign
#define INT16_TO_STRING_MAX 7
// Max number of chars needed to convert signed or unsigned int to char*, including null terminator and possible negative sign
#define INT32_TO_STRING_MAX 12
// Max number of chars needed to convert int64_t OR uint64_t to char*, including null terminator and possible negative sign
#define INT64_TO_STRING_MAX 21
// Number of characters needed for a UUID, including null terminator. See https://www.postgresql.org/docs/11/datatype-uuid.html
#define UUID_CHARACTER_LENGTH 37

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

// Copy a literal using strcpy and do bounds checking
#define	STRCPY_LIT(TARGET, LITERAL, MAX_LEN)				\
{									\
	assert(MAX_LEN >= strlen(LITERAL));				\
	strcpy(TARGET, LITERAL);					\
}

// Increase the amount of memory allocated for a give array by the size specified by NEW_SIZE,
// preserving the values from ARRAY and updating SIZE to NEW_SIZE
#define EXPAND_ARRAY_ALLOCATION(ARRAY, SIZE, NEW_SIZE, TYPE)				\
{											\
	TYPE *new_array;								\
	new_array = (TYPE*)malloc(NEW_SIZE * sizeof(TYPE));				\
	memcpy(new_array, ARRAY, SIZE * sizeof(TYPE));					\
	memset(&new_array[SIZE], 0, (NEW_SIZE - SIZE) * sizeof(TYPE));			\
	free(ARRAY);									\
	ARRAY = new_array;								\
	SIZE = NEW_SIZE;								\
}

// Double the amount of memory allocated for a given array and copy in values from previous allocation
// and zero-initialize remaining memory
#define DOUBLE_ARRAY_ALLOCATION(ARRAY, SIZE, TYPE)				\
{										\
	TYPE *new_array;							\
	new_array = (TYPE*)malloc(SIZE * 2 * sizeof(TYPE));			\
	memcpy(new_array, ARRAY, SIZE * sizeof(TYPE));				\
	memset(&new_array[SIZE], 0, SIZE * sizeof(TYPE));			\
	free(ARRAY);								\
	ARRAY = new_array;							\
	SIZE = SIZE * 2;							\
}

// Convert a 16-bit integer to a string and store in a ydb_buffer_t
#define OCTO_INT16_TO_BUFFER(INT16, BUFFERP)									\
{														\
	(BUFFERP)->len_used = snprintf((BUFFERP)->buf_addr, INT16_TO_STRING_MAX, "%d", INT16);			\
	assert(sizeof(INT16) == sizeof(int16_t));									\
	assert((BUFFERP)->len_alloc >= (BUFFERP)->len_used);							\
	assert(INT16_TO_STRING_MAX >= (BUFFERP)->len_used);	/* The result should never be truncated. */	\
	(BUFFERP)->buf_addr[(BUFFERP)->len_used] = '\0';	/* Often reuse this string, so add null. */	\
}

// Convert a 32-bit integer to a string and store in a ydb_buffer_t
#define OCTO_INT32_TO_BUFFER(INT32, BUFFERP)									\
{														\
	(BUFFERP)->len_used = snprintf((BUFFERP)->buf_addr, INT32_TO_STRING_MAX, "%d", INT32);			\
	assert(sizeof(INT32) == sizeof(int32_t));									\
	assert((BUFFERP)->len_alloc >= (BUFFERP)->len_used);							\
	assert(INT32_TO_STRING_MAX >= (BUFFERP)->len_used);	/* The result should never be truncated. */	\
	(BUFFERP)->buf_addr[(BUFFERP)->len_used] = '\0';	/* Often reuse this string, so add null. */	\
}

// Convert a 64-bit integer to a string and store in a ydb_buffer_t
#define OCTO_INT64_TO_BUFFER(INT64, BUFFERP)									\
{														\
	(BUFFERP)->len_used = snprintf((BUFFERP)->buf_addr, INT64_TO_STRING_MAX, "%lld", INT64);		\
	assert(sizeof(INT64) == sizeof(int64_t));									\
	assert((BUFFERP)->len_alloc >= (BUFFERP)->len_used);							\
	assert(INT64_TO_STRING_MAX >= (BUFFERP)->len_used);	/* The result should never be truncated. */	\
	(BUFFERP)->buf_addr[(BUFFERP)->len_used] = '\0';	/* Often reuse this string, so add null. */	\
}

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
		FATAL(ERR_BUFFER_TOO_SMALL, "output plan");								\
	}													\
}

#define	INVOKE_HASH_CANONICAL_QUERY(STATE, RESULT, STATUS)	\
{								\
	STATUS = 0;						\
	HASH128_STATE_INIT(STATE, 0);				\
	hash_canonical_query_cycle++;				\
	hash_canonical_query(&STATE, RESULT, &STATUS);		\
}

#define LOG_LOCAL_ONLY(SEVERITY, ERROR, ...)			\
{								\
	/* Disable message sending only if already enabled */	\
	if (!rocto_session.sending_message) {			\
		rocto_session.sending_message = TRUE;		\
		SEVERITY(ERROR, ## __VA_ARGS__);		\
		rocto_session.sending_message = FALSE;		\
	} else {						\
		SEVERITY(ERROR, ## __VA_ARGS__);		\
	}							\
}

#define INVOKE_REGEX_SPECIFICATION(STMT, OP0, OP1, IS_REGEX_LIKE_OR_SIMILAR, IS_SENSITIVE, IS_NOT, PARSE_CONTEXT)	\
{															\
	int status;													\
	status = regex_specification(STMT, OP0, OP1, IS_REGEX_LIKE_OR_SIMILAR, IS_SENSITIVE, IS_NOT, PARSE_CONTEXT);	\
	if (0 != status)												\
		YYABORT;												\
}

#define INVOKE_PARSE_LITERAL_TO_PARAMETER(PARSE_CONTEXT, VALUE_STMT, UPDATE_EXISTING)		\
{												\
	int status;										\
	status = parse_literal_to_parameter(PARSE_CONTEXT, VALUE_STMT, UPDATE_EXISTING);	\
	if (0 != status)									\
		YYABORT;									\
}

// Initialize a stack-allocated ydb_buffer_t from a stack-allocated string (char *)
#define OCTO_SET_BUFFER(BUFFER, STRING)					\
{									\
	BUFFER.buf_addr = STRING;					\
	BUFFER.len_alloc = sizeof(STRING);				\
}

/* Below are special values used as part of the `aggregate_depth` parameter passed to `qualify_statement()`
 * to indicate we are qualifying a FROM clause, WHERE clause, GROUP BY clause etc.. These need to be negative as 0 is the
 * first valid depth used in the normal case (e.g. when qualifying the SELECT column list i.e. select->select_list) etc.
 * Some of these negative values indicate clauses where Aggregate functions are disallowed (i.e. FROM and WHERE).
 */
#define	AGGREGATE_DEPTH_FROM_CLAUSE		-1
#define	AGGREGATE_DEPTH_WHERE_CLAUSE		-2
#define	AGGREGATE_DEPTH_GROUP_BY_CLAUSE		-3

// Convenience type definition for run_query callback function
typedef int (*callback_fnptr_t)(SqlStatement *, int, void*, char*, boolean_t);

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
SqlStatement *parse_line(ParseContext *parse_context);

int populate_data_type(SqlStatement *v, SqlValueType *type, ParseContext *parse_context);
SqlValueType get_sqlvaluetype_from_psql_type(PSQL_TypeOid type);
PSQL_TypeOid get_psql_type_from_sqlvaluetype(SqlValueType type);
PSQL_TypeSize get_type_size_from_psql_type(PSQL_TypeOid type);
SqlTable *find_table(const char *table_name);
int drop_table_from_local_cache(ydb_buffer_t *table_name_buffer);
SqlColumn *find_column(char *column_name, SqlTable *table);
SqlStatement *find_column_alias_name(SqlStatement *stmt);
int qualify_query(SqlStatement *table_alias_stmt, SqlJoin *parent_join, SqlTableAlias *parent_table_alias);
int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *table_alias_stmt,
							int depth, SqlColumnListAlias **ret_cla);
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *table_alias_stmt,
									int depth, SqlColumnListAlias **ret_cla);
SqlColumnListAlias *match_column_in_table(SqlTableAlias *table, char *column_name, int column_name_len,
									boolean_t *ambiguous, boolean_t issue_error);
boolean_t match_column_list_alias_in_select_column_list(SqlColumnListAlias *match_cla, SqlStatement *cla_stmt);
int qualify_function_name(SqlStatement *stmt);
void print_yyloc(YYLTYPE *llocp);
SqlOptionalKeyword *get_keyword(SqlColumn *column, enum OptionalKeyword keyword);
SqlOptionalKeyword *get_keyword_from_keywords(SqlOptionalKeyword *start_keyword, enum OptionalKeyword keyword);
int get_key_columns(SqlTable *table, SqlColumn **key_columns);
int generate_key_name(char *buffer, int buffer_size, int target_key_num, SqlTable *table, SqlColumn **key_columns);
int print_temporary_table(SqlStatement *, int cursor_id, void *parms, char *plan_name, boolean_t send_row_description);
void print_result_row(ydb_buffer_t *row);
int get_mval_len(unsigned char *buff, int *data_len);

/**
 * Parses query, and calls the callback if it is a select statement. Otherwise, the query is a data altering
 *  query and gets executed
 *
 * @returns TRUE on success, FALSE on failure
 */
int run_query(callback_fnptr_t callback, void *parms, boolean_t send_row_description, ParseContext *parse_context);

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
boolean_t match_sql_statement(SqlStatement *stmt, SqlStatement *match_stmt);

void compress_statement(SqlStatement *stmt, char **out, int *out_length);
SqlStatement *decompress_statement(char *buffer, int out_length);
int store_table_in_pg_class(SqlTable *table, ydb_buffer_t *table_name_buffer);
int delete_table_from_pg_class(ydb_buffer_t *table_name_buffer);
void cleanup_tables();

/* Parse related functions invoked from the .y files (parser.y, select.y etc.) */
SqlStatement *aggregate_function(SqlAggregateType aggregate_type, OptionalKeyword set_quantifier, SqlStatement *value_expression);
SqlStatement *between_predicate(SqlStatement *row_value_constructor, SqlStatement *from, SqlStatement *to, boolean_t not_specified);
SqlStatement *cast_specification(SqlStatement *cast_specification, SqlStatement *source);
SqlStatement *grouping_column_reference(SqlStatement *derived_column_expression, SqlStatement *collate_clause);
int natural_join_condition(SqlJoin *start, SqlJoin *r_join);
int parse_literal_to_parameter(ParseContext *parse_context, SqlValue *value, boolean_t update_existing);
SqlStatement *query_specification(OptionalKeyword set_quantifier, SqlStatement *select_list,
					SqlStatement *table_expression, SqlStatement *sort_specification_list, int *plan_id);
int regex_specification(SqlStatement **stmt, SqlStatement *op0, SqlStatement *op1, int is_regex_like_or_similar,
									int is_sensitive, int is_not, ParseContext *parse_context);
SqlStatement *set_operation(enum SqlSetOperationType setoper_type, SqlStatement *left_operand, SqlStatement *right_operand);
SqlStatement *sort_specification(SqlStatement *sort_key, SqlStatement *ordering_specification);
SqlStatement *table_reference(SqlStatement *column_name, SqlStatement *correlation_specification,
							SqlStatement *table_reference_tail, int *plan_id);

/* trims duplicate '.*'s from regex */
void trim_dot_star(SqlValue *regex);

// Creates a new cursor by assigning a new cursorId
int64_t create_cursor(ydb_buffer_t *schema_global, ydb_buffer_t *cursor_buffer);
boolean_t is_query_canceled(callback_fnptr_t callback, int32_t cursorId, void *parms,
		char *plan_name, boolean_t send_row_description);


/**
 * Returns TRUE if the columns are equal, FALSE otherwise
 */
int columns_equal(SqlColumn *a, SqlColumn *b);
int tables_equal(SqlTable *a, SqlTable *b);
int values_equal(SqlValue *a, SqlValue *b);

// Returns the amount of memory used by the current process
int64_t get_mem_usage();

int no_more();

int get_input(char *buf, int size);
void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, ParseContext *parse_context, char const *s);

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
#endif
