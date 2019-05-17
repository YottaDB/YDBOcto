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

#include <libyottadb.h>

#include "errors.h"
#include "octo_types.h"
#include "config.h"
#include "constants.h"
#include "memory_chunk.h"

#include "mmrhash.h"

#if YDB_RELEASE < 125
/* Macro to copy a string (i.e. "char *" pointer in C) to an already allocated ydb_buffer_t structure.
 * If BUFFERP does not have space allocated to hold STRING, then no copy is done
 *	and COPY_DONE will be set to FALSE.
 * Else the copy is done and COPY_DONE will be set to TRUE.
 * User of this macro needs to include <string.h> (needed for "strlen" prototype).
 * See comment before YDB_STRING_TO_BUFFER macro for why YDB_COPY_LITERAL_TO_BUFFER and YDB_COPY_STRING_TO_BUFFER
 * cannot be merged into one macro (i.e. sizeof is not same as strlen in rare case).
 */
#define YDB_COPY_STRING_TO_BUFFER(STRING, BUFFERP, COPY_DONE)	\
{								\
	int	len;						\
								\
	len = strlen(STRING);					\
	if (len <= (BUFFERP)->len_alloc)			\
	{							\
		memcpy((BUFFERP)->buf_addr, STRING, len);	\
		(BUFFERP)->len_used = len;			\
		COPY_DONE = TRUE;				\
	} else							\
		COPY_DONE = FALSE;				\
}

/* Macro to create/fill-in a ydb_buffer_t structure from a C string (char * pointer).
 * Note that YDB_LITERAL_TO_BUFFER does a "sizeof(LITERAL) - 1" whereas YDB_STRING_TO_BUFFER does a "strlen()".
 * Both produce the same output almost always. There is one exception though and that is if LITERAL has embedded null bytes
 * in it. In that case, sizeof() would include the null bytes too whereas strlen() would not. Hence the need for both versions
 * of the macros.
 */
#define YDB_STRING_TO_BUFFER(STRING, BUFFERP)				\
{									\
	(BUFFERP)->buf_addr = STRING;					\
	(BUFFERP)->len_used = (BUFFERP)->len_alloc = strlen(STRING);	\
}
#endif

/**
 * Switches to the octo global directory
 */
#define SWITCH_TO_OCTO_GLOBAL_DIRECTORY()						\
	do {										\
		int status = 0;								\
		ydb_buffer_t z_status, z_status_value;					\
		status = ydb_get_s(&config->zgbldir, 0, NULL, &config->prev_gbldir);	\
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);			\
		status = ydb_set_s(&config->zgbldir, 0, NULL, &config->octo_gbldir);	\
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);			\
	} while(FALSE);

/**
 * Switches from the octo global directory
 */
#define SWITCH_FROM_OCTO_GLOBAL_DIRECTORY()						\
	do {										\
		int status = 0;								\
		ydb_buffer_t z_status, z_status_value;					\
		status = ydb_set_s(&config->zgbldir, 0, NULL, &config->prev_gbldir);	\
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);			\
	} while(FALSE);

// Allows us to leave parameters in place even if they are unused and avoid warning from the
// compiler.
#define UNUSED(x) (void)(x)

// Set maximum M routine length - must be in sync with MAX_MIDENT_LEN in YDB/sr_port/mdef.h
#define MAX_ROUTINE_LEN 31

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
int qualify_column_list_alias(SqlColumnListAlias *alias, SqlJoin *tables, SqlStatement *column_list_alias);
int qualify_column_list(SqlColumnList *select_columns, SqlJoin *tables, SqlStatement *column_list_alias);
SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *column_list_alias);
SqlStatement *match_column_in_table(SqlTableAlias *table, char *column_name, int column_name_len);
int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *column_list_alias);
int qualify_join_conditions(SqlJoin *join, SqlJoin *tables, SqlStatement *column_list_alias);
int qualify_function_name(SqlStatement *stmt);
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
#endif
