/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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

#define	MATCH_QUALIFIED_COLUMNS_FALSE	FALSE
#define	MATCH_QUALIFIED_COLUMNS_TRUE	TRUE

#define	INVOKE_HASH_CANONICAL_QUERY(STATE, RESULT, STATUS)	\
{								\
	STATUS = 0;						\
	HASH128_STATE_INIT(STATE, 0);				\
	hash_canonical_query_cycle++;				\
	hash_canonical_query(&STATE, RESULT, &STATUS);		\
}

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
SqlStatement *parse_line();

int populate_data_type(SqlStatement *v, SqlValueType *type);
SqlTable *find_table(const char *table_name);
SqlColumn *find_column(char *column_name, SqlTable *table);
SqlStatement *find_column_alias_name(SqlStatement *stmt);
int qualify_column_list_alias(SqlColumnListAlias *alias, SqlJoin *tables, SqlStatement *column_list_alias,
					boolean_t match_qualified_columns);
int qualify_column_list(SqlColumnList *select_columns, SqlJoin *tables, SqlStatement *column_list_alias,
					boolean_t match_qualified_columns);
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *column_list_alias,
					boolean_t match_qualified_columns);
SqlStatement *match_column_in_table(SqlTableAlias *table, char *column_name, int column_name_len,
					boolean_t match_qualified_columns);
int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *column_list_alias, boolean_t match_qualified_columns);
int qualify_function_name(SqlStatement *stmt);
int qualify_query(SqlStatement *stmt, SqlJoin *parent_join);
void print_yyloc(YYLTYPE *llocp);
SqlOptionalKeyword *get_keyword(SqlColumn *column, enum OptionalKeyword keyword);
SqlOptionalKeyword *get_keyword_from_keywords(SqlOptionalKeyword *start_keyword, enum OptionalKeyword keyword);
int get_key_columns(SqlTable *table, SqlColumn **key_columns);
int generate_key_name(char *buffer, int buffer_size, int target_key_num, SqlTable *table, SqlColumn **key_columns);

char *like_to_regex(const char *src);
char *similar_to_regex(const char *src);

/* Hashing support functions */
int generate_routine_name(hash128_state_t *state, char *routine_name, int routine_len, FileType file_type);
void hash_canonical_query(hash128_state_t *state, SqlStatement *stmt, int *status);
void ydb_hash_to_string(ydb_uint16 *hash, char *buffer, const unsigned int buf_len);

void assign_table_to_columns(SqlStatement *table_statement);
SqlOptionalKeyword *add_optional_piece_keyword_to_sql_column(int column_number);

// Converts a list of columns to a column list associated with the given table alias
SqlColumnListAlias *columns_to_column_list_alias(SqlColumn *column, SqlTableAlias *table_alias);
SqlColumn *column_list_alias_to_columns(SqlTableAlias *table_alias);

SqlStatement *drill_to_table_alias(SqlStatement *sqlStmt);
int get_column_piece_number(SqlColumnAlias *alias, SqlTableAlias *table_alias);

SqlStatement *copy_sql_statement(SqlStatement *stmt);

void compress_statement(SqlStatement *stmt, char **out, int *out_length);
SqlStatement *decompress_statement(char *buffer, int out_length);
int store_table_in_pg_class(SqlTable *table);
void cleanup_tables();

/* Parse related functions invoked from the .y files (parser.y, select.y etc.) */
SqlStatement *query_specification(SqlStatement *set_quantifier, SqlStatement *select_list,
					SqlStatement *table_expression, SqlStatement *sort_specification_list, int *plan_id);
SqlStatement *sort_specification(SqlStatement *sort_key, SqlStatement *collate_clause, SqlStatement *ordering_specification);
void regex_specification(SqlStatement **stmt, SqlStatement *op0, SqlStatement *op1, int is_regex_like_or_similar, int is_sensitive, int is_not);

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
int		cur_input_max;
int		cancel_received;
int		eof_hit;
FILE		*inputFile;
FILE		*err_buffer;
char		*input_buffer_combined;		// The input buffer for octo. Contains the query strings.
int		(*cur_input_more)();

int get_input(char *buf, int size);
void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, char const *s);
#endif
