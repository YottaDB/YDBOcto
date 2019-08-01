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

// Specify character for delimiting column values in Octo
#define COLUMN_DELIMITER "|"

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

int emit_column_specification(char *buffer, int buffer_size, SqlColumn *column);
void emit_create_table(FILE *output, struct SqlStatement *stmt);
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
SqlStatement *parse_line(const char *line);

int populate_data_type(SqlStatement *v, SqlValueType *type);
SqlTable *find_table(const char *table_name);
SqlColumn *find_column(char *column_name, SqlTable *table);
SqlStatement *find_column_alias_name(SqlStatement *stmt);
int qualify_column_list_alias(SqlColumnListAlias *alias, SqlJoin *tables, SqlStatement *column_list_alias);
int qualify_column_list(SqlColumnList *select_columns, SqlJoin *tables, SqlStatement *column_list_alias);
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *column_list_alias);
SqlStatement *match_column_in_table(SqlTableAlias *table, char *column_name, int column_name_len);
int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *column_list_alias);
int qualify_join_conditions(SqlJoin *join, SqlJoin *tables, SqlStatement *column_list_alias);
int qualify_function_name(SqlStatement *stmt);
int qualify_query(SqlStatement *stmt, SqlJoin *parent_join);
void print_yyloc(YYLTYPE *llocp);
SqlOptionalKeyword *get_keyword(SqlColumn *column, enum OptionalKeyword keyword);
SqlOptionalKeyword *get_keyword_from_keywords(SqlOptionalKeyword *start_keyword, enum OptionalKeyword keyword);
int get_key_columns(SqlTable *table, SqlColumn **key_columns);
int generate_key_name(char *buffer, int buffer_size, int target_key_num, SqlTable *table, SqlColumn **key_columns);

char *regex_to_like(const char *src);

/* Hashing support functions */
int generate_routine_name(hash128_state_t *state, char *routine_name, int routine_len, FileType file_type);
void hash_canonical_query(hash128_state_t *state, SqlStatement *stmt);
void ydb_hash_to_string(ydb_uint16 *hash, char *buffer, const unsigned int buf_len);

void assign_table_to_columns(SqlStatement *table_statement);
SqlColumn *column_list_alias_to_columns(SqlTableAlias *table_alias);
int get_column_piece_number(SqlColumnAlias *alias, SqlTableAlias *table_alias);
SqlStatement *replace_table_references(SqlStatement *stmt, SqlStatement *to_remove);
SqlStatement *update_table_references(SqlStatement *stmt, int old_unique_id, int new_unique_id);

SqlStatement *copy_sql_statement(SqlStatement *stmt);

void compress_statement(SqlStatement *stmt, char **out, int *out_length);
SqlStatement *decompress_statement(char *buffer, int out_length);
int store_table_in_pg_class(SqlTable *table);
void cleanup_tables();

/**
 * Returns TRUE if the columns are equal, FALSE otherwise
 */
int columns_equal(SqlColumn *a, SqlColumn *b);
int tables_equal(SqlTable *a, SqlTable *b);
int values_equal(SqlValue *a, SqlValue *b);

int no_more();

/* Globals */
SqlTable *definedTables;
int cur_input_index;
int cur_input_max;
int eof_hit;
FILE *inputFile;
FILE *err_buffer;
char *input_buffer_combined;
int (*cur_input_more)();

int get_input(char *buf, int size);
void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, char const *s);
#endif
