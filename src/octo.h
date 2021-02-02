/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
#include <limits.h>    /* needed for PATH_MAX */
#include <sys/param.h> /* needed for PATH_MAX */
#include <stdint.h>    /* needed for uint64_t */
#include <errno.h>     /* for errno */
#include <ctype.h>     /* for tolower/toupper */

#include <libyottadb.h>

#include "rocto/message_formats.h"
#include "errors.h"
#include "octo_types.h"
#include "config.h"
#include "constants.h"
#include "memory_chunk.h"

#include "mmrhash.h"

// Max number of chars needed to convert signed or unsigned 8-bit int to char*, including null terminator and possible negative sign
#define INT8_TO_STRING_MAX 5
// Max number of chars needed to convert signed or unsigned 16-bit int to char*, including null terminator and possible negative
// sign
#define INT16_TO_STRING_MAX 7
// Max number of chars needed to convert signed or unsigned 32-bit int to char*, including null terminator and possible negative
// sign
#define INT32_TO_STRING_MAX 12
// Max number of chars needed to convert int64_t OR uint64_t to char*, including null terminator and possible negative sign
#define INT64_TO_STRING_MAX 21
// Number of characters needed for a UUID, including null terminator. See https://www.postgresql.org/docs/11/datatype-uuid.html
#define UUID_CHARACTER_LENGTH 37
// Max ASCII value to accept for $CHAR arguments in delimiter
#define DELIM_MAX	     127
#define DOLLAR_CHAR_MAX_ARGS 255
// Distinguish between the cases of a DELIM being either a literal or a $CHAR call
#define DELIM_IS_LITERAL     1
#define DELIM_IS_DOLLAR_CHAR 2

/* Default buffer length to use when creating new buffers. Such buffers will be expanded as needed, so a comparatively small number
 * can be used initially.
 */
#define OCTO_INIT_BUFFER_LEN 1024

// Initial size to allocate for M routine buffers (1 MiB)
#define INIT_M_ROUTINE_LENGTH (1 * 1024 * 1024)
// Initial size to allocate for memory chunks (1 MiB)
#define MEMORY_CHUNK_SIZE (1 * 1024 * 1024)
// Maximum number of key columns
#define MAX_KEY_COUNT 255

/* Maximum query string length for all Octo queries. Currently set to YDB_MAX_STR (the maximum size of a GVN/LVN value) since query
 * strings are stored in LVNs during processing and so are constrained the size limit for LVN values. Should users require a greater
 * maximum query length, this limit will need to be revised and a method devised for storing strings that exceed YDB_MAX_STR.
 */
#define OCTO_MAX_QUERY_LEN YDB_MAX_STR
/* Maximum table or column name length, mirroring the PostgreSQL default of 63 (64 - null terminator):
 * https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS
 */
#define OCTO_MAX_IDENT 63

// Maximum size for a fragment of an extended query or literal parameter value stored in the database
#define MAX_PARM_VALUE_FRAGMENT_SIZE 32768
// Maximum length of an M line
#define M_LINE_MAX 32768

/* Set OCTO_PATH_MAX to be the same as the system PATH_MAX (should be defined by limits.h or sys/param.h)
 * but in case it is not available, set it to a value of 1024 just like is done in YDB.
 */
#ifndef PATH_MAX
#define OCTO_PATH_MAX 1024
#else
#define OCTO_PATH_MAX PATH_MAX
#endif

/* The below macros capture the different states of the "eof_hit" global variable */
#define EOF_NONE  0 /* EOF has not yet been signaled */
#define EOF_CTRLD 1 /* Ctrl-D signaled the Octo process to terminate */
#define EOF_EXIT  2 /* EXIT or QUIT commands signaled the Octo process to terminate */

// Allows us to leave parameters in place even if they are unused and avoid warning from the
// compiler.
#define UNUSED(x) (void)(x)

/* Set maximum trigger name length
 * This cannot be greater than MAX_USER_TRIGNAME_LEN defined in https://gitlab.com/YottaDB/DB/YDB/blob/master/sr_unix/trigger.h
 */
#define MAX_TRIGGER_LEN 28

// Specify character for delimiting column values in Octo
#define COLUMN_DELIMITER "|"

#define VALUES_COLUMN_NAME_PREFIX "COLUMN"

/* Define various string literals used as gvn subscripts and/or in physical plans (generated M code).
 * Each macro is prefixed with a OCTOLIT_.
 * A similar piece of code exists in template_helpers.h where PP_* macros are defined.
 */
#define OCTOLIT_KEYS	    "keys"	  /* keep this in sync with PP_KEYS in "template_helpers.h" */
#define OCTOLIT_PARAMETERS  "parameters"  /* keep this in sync with PP_PARAMETERS in "template_helpers.h" */
#define OCTOLIT_ROW_COUNT   "RowCount"	  /* keep this in sync with PP_ROW_COUNT in "template_helpers.h" */
#define OCTOLIT_VARIABLES   "variables"	  /* keep this in sync with PP_VARIABLES in "template_helpers.h" */
#define OCTOLIT_XREF_STATUS "xref_status" /* keep this in sync with PP_XREF_STATUS in "template_helpers.h" */

#define OCTOLIT_0		     "0"
#define OCTOLIT_ALL		     "all"
#define OCTOLIT_BINARY		     "binary"
#define OCTOLIT_BINFMT		     "binfmt"
#define OCTOLIT_BOUND		     "bound"
#define OCTOLIT_CHUNK		     "chunk"
#define OCTOLIT_COLUMN_ID	     "column_id"
#define OCTOLIT_DATA_TYPE	     "data_type"
#define OCTOLIT_DATA_TYPE_SIZE	     "data_type_size"
#define OCTOLIT_DDL		     "ddl"
#define OCTOLIT_FILES		     "files"
#define OCTOLIT_FUNCTIONS	     "functions"
#define OCTOLIT_FORMAT_CODE	     "format_code"
#define OCTOLIT_LENGTH		     "length"
#define OCTOLIT_TEXT_LENGTH	     "text_length"
#define OCTOLIT_NAME		     "name"
#define OCTOLIT_NONE		     "none"
#define OCTOLIT_OID		     "oid"
#define OCTOLIT_OCTOONEROWTABLE	     "OCTOONEROWTABLE"
#define OCTOLIT_OUTPUT_COLUMNS	     "output_columns"
#define OCTOLIT_OUTPUT_KEY	     "output_key"
#define OCTOLIT_PG_ATTRIBUTE	     "pg_attribute"
#define OCTOLIT_PG_CATALOG	     "pg_catalog"
#define OCTOLIT_PG_CLASS	     "pg_class"
#define OCTOLIT_PLANDIRS	     "plandirs"
#define OCTOLIT_PLANFMT		     "planfmt"
#define OCTOLIT_PLAN_METADATA	     "plan_metadata"
#define OCTOLIT_PREPARED	     "prepared"
#define OCTOLIT_ROUTINE		     "routine"
#define OCTOLIT_SEEDFMT		     "seedfmt"
#define OCTOLIT_T		     "t"
#define OCTOLIT_TABLEPLANS	     "tableplans"
#define OCTOLIT_TABLES		     "tables"
#define OCTOLIT_TABLE_ID	     "table_id"
#define OCTOLIT_TAG		     "tag"
#define OCTOLIT_TEXT		     "text"
#define OCTOLIT_TIMESTAMP	     "timestamp"
#define OCTOLIT_TYPE_MODIFIER	     "type_modifier"
#define OCTOLIT_USER		     "user"
#define OCTOLIT_USERS		     "users"
#define OCTOLIT_YDBOCTOCANCEL	     "%ydboctoCancel"
#define OCTOLIT_YDBOCTOSECRETKEYLIST "%ydboctoSecretKeyList"

// Macros for plan size allocation in physical plan generation
#define OCTOPLAN_LIT	  "octoPlan"
#define XREFPLAN_LIT	  "xrefPlan"
#define MAX_PLAN_NAME_LEN sizeof(OCTOPLAN_LIT) + INT32_TO_STRING_MAX

/* Set maximum command tag length for use in extended query protocol, including null terminator
 * This value should be large enough to hold the longest possible first keyword of a SQL query, i.e. "DEALLOCATE" i.e. 10 bytes
 * In addition, in case of a "SELECT", the tag would be followed by a space and a 4-byte integer count (number of rows returned).
 * And in case of a "INSERT", the tag would be followed by 2 spaces and 2 4-byte integers so account for those.
 */
#define MAX_FIRST_KEYWORD_OF_SQL_QUERY_LEN 10 /* size of "DEALLOCATE" */

#define MAX_TAG_LEN MAX_FIRST_KEYWORD_OF_SQL_QUERY_LEN + (1 + INT32_TO_STRING_MAX) + (1 + INT32_TO_STRING_MAX) + 1

/* For INSERT INTO, Octo conforms to Postgres output format (see https://www.postgresql.org/docs/9.5/sql-insert.html for details).
 * Relevant part pasted below.
 * ---------------------------------------------------------------------------------
 * On successful completion, an INSERT command returns a command tag of the form
 *	INSERT oid count
 * The count is the number of rows inserted or updated. If count is exactly one, and the target
 * table has OIDs, then oid is the OID assigned to the inserted row. The single row must have
 * been inserted rather than updated. Otherwise oid is zero.
 * ---------------------------------------------------------------------------------
 * Note: In the case of Octo, the "oid" is always 0 (and it is not expected to change in the future either)
 * hence the 0 in the macro below.
 */
#define INSERT_COMMAND_TAG "INSERT 0"

// Default buffer allocated for $zroutines
#define ZRO_INIT_ALLOC 512

/* Maximum size of each fragment a table or function binary or text definition is split into in M nodes */
#define MAX_DEFINITION_FRAGMENT_SIZE 32768

#define YDB_MAX_KEY_SZ 1023 /* 1023 can be replaced by MAX_STR_LEN (from YDB repo) if it is exposed in libyottadb.h */

/* Size of query buffer initially allocated. Gets expanded as need arises. */
#define INIT_QUERY_SIZE 32768

/* The below macro needs to be manually bumped if binary table OR function definition format changes due to
 * dependent structure layout changes (e.g. SqlStatement structure etc.).
 * The "test-auto-upgrade" pipeline job (that automatically runs) will alert us if it detects the need for the bump.
 * And that is considered good enough for now (i.e. no manual review of code necessary to detect the need for a bump).
 */
#define FMT_BINARY_DEFINITION 5

/* The below macro needs to be manually bumped if at least one of the following changes.
 *	1) Generated physical plan (_ydboctoP*.m) file name OR contents
 *	2) Generated cross-reference/xref plan (_ydboctoX*.m) file name OR contents (if any)
 *	3) Generated trigger (associated with a cross-reference/xref plan) name OR contents (if any)
 * The "test-auto-upgrade" pipeline job (that automatically runs) will alert us if it detects the need for the bump.
 * And that is considered good enough for now (i.e. no manual review of code necessary to detect the need for a bump).
 */
#define FMT_PLAN_DEFINITION 7

// Below are a few utility macros that are similar to those defined in sr_port/gtm_common_defs.h.
// But we do not use those as that is outside the control of Octo's source code repository
// Therefore we undef those macros (if any defined) and redefine it ourselves here.

#undef LIT_LEN	  // in case it gets defined in sr_port/gtm_common_defs.h (which is included by mmrhash.h above)
#undef MEMCMP_LIT // would have been defined by sr_port/gtm_common_defs.h (which is included by mmrhash.h above)
#undef MEMCPY_LIT // would have been defined by sr_port/gtm_common_defs.h (which is included by mmrhash.h above)

// Returns the length of a string literal (similar to STR_LIT_LEN in sr_port/gtm_common_defs.h)
#define LIT_LEN(LITERAL) (sizeof(LITERAL) - 1)

// Do a memcmp of a source string against a string literal
#define MEMCMP_LIT(SOURCE, LITERAL) memcmp(SOURCE, LITERAL, sizeof(LITERAL) - 1)

// Do a memcpy onto a string target using a string literal as the source
#define MEMCPY_LIT(TARGET, LITERAL) memcpy(TARGET, LITERAL, sizeof(LITERAL) - 1)

// Copy a literal using strcpy and do bounds checking
#define STRCPY_LIT(TARGET, LITERAL, MAX_LEN)        \
	{                                           \
		assert(MAX_LEN >= strlen(LITERAL)); \
		strcpy(TARGET, LITERAL);            \
	}

/* Convert a string to uppercase and store in provided destination.
 * Note that DEST and START may or may not be the same. This macro handles both cases.
 */
#define TOUPPER(DEST, DEST_END, START, END)                                                 \
	{                                                                                   \
		char *start;                                                                \
                                                                                            \
		start = (START);                                                            \
		while (start < END) {                                                       \
			(*DEST) = toupper(*start);                                          \
			(DEST)++;                                                           \
			start++;                                                            \
		}                                                                           \
		(*DEST) = '\0';                                                             \
		(DEST)++;                                                                   \
		/* Check for buffer overflow. It is okay if DEST < DEST_END as there are */ \
		/* cases where this macro is applied piecemeal to a single buffer. */       \
		assert(DEST <= DEST_END);                                                   \
	}

/* Convert a string to lowercase and store in provided destination
 * Note that DEST and START may or may not be the same. This macro handles both cases.
 */
#define TOLOWER(DEST, DEST_END, START, END)        \
	{                                          \
		char *start;                       \
                                                   \
		start = (START);                   \
		while (start < END) {              \
			(*DEST) = tolower(*start); \
			(DEST)++;                  \
			start++;                   \
		}                                  \
		(*DEST) = '\0';                    \
		(DEST)++;                          \
		assert(DEST <= DEST_END);          \
	}

// Convert a string to uppercase in place
#define TOUPPER_STR(STR)                         \
	{                                        \
		size_t len;                      \
		char * end;                      \
                                                 \
		len = strlen(STR);               \
		end = STR + len;                 \
		TOUPPER(STR, end + 1, STR, end); \
	}

// Convert a string to lowercase in place
#define TOLOWER_STR(STR)                         \
	{                                        \
		size_t len;                      \
		char * end;                      \
                                                 \
		len = strlen(STR);               \
		end = STR + len;                 \
		TOLOWER(STR, end + 1, STR, end); \
	}

// Increase the amount of memory allocated for a give array by the size specified by NEW_SIZE,
// preserving the values from ARRAY and updating SIZE to NEW_SIZE
#define EXPAND_ARRAY_ALLOCATION(ARRAY, SIZE, NEW_SIZE, TYPE)                   \
	{                                                                      \
		TYPE *new_array;                                               \
		new_array = (TYPE *)malloc(NEW_SIZE * sizeof(TYPE));           \
		memcpy(new_array, ARRAY, SIZE * sizeof(TYPE));                 \
		memset(&new_array[SIZE], 0, (NEW_SIZE - SIZE) * sizeof(TYPE)); \
		free(ARRAY);                                                   \
		ARRAY = new_array;                                             \
		SIZE = NEW_SIZE;                                               \
	}

// Double the amount of memory allocated for a given array and copy in values from previous allocation
// and zero-initialize remaining memory
#define DOUBLE_ARRAY_ALLOCATION(ARRAY, SIZE, TYPE, MAX)              \
	{                                                            \
		int new_size;                                        \
                                                                     \
		new_size = SIZE * 2;                                 \
		if ((0 < MAX) && (MAX < new_size)) {                 \
			new_size = MAX;                              \
		}                                                    \
		TYPE *new_array;                                     \
		new_array = (TYPE *)malloc(new_size * sizeof(TYPE)); \
		memcpy(new_array, ARRAY, SIZE * sizeof(TYPE));       \
		memset(&new_array[SIZE], 0, SIZE * sizeof(TYPE));    \
		free(ARRAY);                                         \
		ARRAY = new_array;                                   \
		SIZE = new_size;                                     \
	}

/* Increase allocated size of a ydb_buffer_t. Assumes len_used is set to the desired new size as is done
 * when YDB_ERR_INVSTRLEN is encountered.
 */
#define EXPAND_YDB_BUFFER_T_ALLOCATION(BUFFER)                           \
	{                                                                \
		int newsize = BUFFER.len_used;                           \
                                                                         \
		YDB_FREE_BUFFER(&BUFFER);                                \
		YDB_MALLOC_BUFFER(&BUFFER, newsize + 1);                 \
		BUFFER.len_alloc--; /* Leave room for null terminator */ \
	}

// Convert a 16-bit integer to a string and store in a ydb_buffer_t
#define OCTO_INT16_TO_BUFFER(INT16, BUFFERP)                                                                    \
	{                                                                                                       \
		(BUFFERP)->len_used = snprintf((BUFFERP)->buf_addr, INT16_TO_STRING_MAX, "%d", INT16);          \
		assert(sizeof(INT16) == sizeof(int16_t));                                                       \
		assert((BUFFERP)->len_alloc >= (BUFFERP)->len_used);                                            \
		assert(INT16_TO_STRING_MAX >= (BUFFERP)->len_used); /* The result should never be truncated. */ \
		(BUFFERP)->buf_addr[(BUFFERP)->len_used] = '\0';    /* Often reuse this string, so add null. */ \
	}

// Convert a 32-bit integer to a string and store in a ydb_buffer_t
#define OCTO_INT32_TO_BUFFER(INT32, BUFFERP)                                                                    \
	{                                                                                                       \
		(BUFFERP)->len_used = snprintf((BUFFERP)->buf_addr, INT32_TO_STRING_MAX, "%d", INT32);          \
		assert(sizeof(INT32) == sizeof(int32_t));                                                       \
		assert((BUFFERP)->len_alloc >= (BUFFERP)->len_used);                                            \
		assert(INT32_TO_STRING_MAX >= (BUFFERP)->len_used); /* The result should never be truncated. */ \
		(BUFFERP)->buf_addr[(BUFFERP)->len_used] = '\0';    /* Often reuse this string, so add null. */ \
	}

// Convert a 64-bit integer to a string and store in a ydb_buffer_t
#define OCTO_INT64_TO_BUFFER(INT64, BUFFERP)                                                                    \
	{                                                                                                       \
		(BUFFERP)->len_used = snprintf((BUFFERP)->buf_addr, INT64_TO_STRING_MAX, "%lld", INT64);        \
		assert(sizeof(INT64) == sizeof(int64_t));                                                       \
		assert((BUFFERP)->len_alloc >= (BUFFERP)->len_used);                                            \
		assert(INT64_TO_STRING_MAX >= (BUFFERP)->len_used); /* The result should never be truncated. */ \
		(BUFFERP)->buf_addr[(BUFFERP)->len_used] = '\0';    /* Often reuse this string, so add null. */ \
	}

// Helper macro: adds integer values (statement type values, unique_id, column_number etc.) to hash digest
// This is kept as a macro as that way we can assert that the size of FIELD is the same as the size of an "int".
// If that assert fails, it is time to use "long" or some other type based on the size of the input.
#define ADD_INT_HASH(STATE, FIELD)                                                               \
	{                                                                                        \
		int lclInt; /* needed in case FIELD is a constant (cannot take & on constant) */ \
                                                                                                 \
		assert(sizeof(FIELD) == sizeof(int));                                            \
		lclInt = FIELD;                                                                  \
		ydb_mmrhash_128_ingest(STATE, (void *)&lclInt, sizeof(lclInt));                  \
	}

#define INVOKE_HASH_CANONICAL_QUERY(STATE, RESULT, STATUS)     \
	{                                                      \
		STATUS = 0;                                    \
		HASH128_STATE_INIT(STATE, 0);                  \
		hash_canonical_query_cycle++;                  \
		hash_canonical_query(&STATE, RESULT, &STATUS); \
	}

#define LOG_LOCAL_ONLY(SEVERITY, ERROR, ...)                          \
	{                                                             \
		/* Disable message sending only if already enabled */ \
		if (!rocto_session.sending_message) {                 \
			rocto_session.sending_message = TRUE;         \
			SEVERITY(ERROR, ##__VA_ARGS__);               \
			rocto_session.sending_message = FALSE;        \
		} else {                                              \
			SEVERITY(ERROR, ##__VA_ARGS__);               \
		}                                                     \
	}

/* Below enum defines type of regex operation */
typedef enum RegexType {
	REGEX_LIKE = 1,
	REGEX_SIMILARTO,
	REGEX_TILDE,
} RegexType;

#define INVOKE_QUERY_SPECIFICATION(Q_SPEC, SET_QT, SELECT_LIST, TABLE_EXPR, SORT_SPEC_LIST, PLAN_ID)    \
	{                                                                                               \
		Q_SPEC = query_specification(SET_QT, SELECT_LIST, TABLE_EXPR, SORT_SPEC_LIST, PLAN_ID); \
		if (NULL == Q_SPEC)                                                                     \
			YYABORT;                                                                        \
	}

#define INVOKE_REGEX_SPECIFICATION(STMT, OP0, OP1, IS_REGEX_LIKE_OR_SIMILAR, IS_SENSITIVE, IS_NOT, PARSE_CONTEXT)            \
	{                                                                                                                    \
		int status;                                                                                                  \
                                                                                                                             \
		status = regex_specification(STMT, OP0, OP1, IS_REGEX_LIKE_OR_SIMILAR, IS_SENSITIVE, IS_NOT, PARSE_CONTEXT); \
		if (0 != status)                                                                                             \
			YYABORT;                                                                                             \
	}

#define INVOKE_PARSE_LITERAL_TO_PARAMETER(PARSE_CONTEXT, VALUE_STMT, UPDATE_EXISTING)            \
	{                                                                                        \
		int status;                                                                      \
		status = parse_literal_to_parameter(PARSE_CONTEXT, VALUE_STMT, UPDATE_EXISTING); \
		if (0 != status)                                                                 \
			YYABORT;                                                                 \
	}

/* Below macro assumes VALUE_STMT->type is value_STATEMENT */
#define IS_LITERAL_PARAMETER(VALUE_TYPE)                                                                     \
	((NUMERIC_LITERAL == VALUE_TYPE) || (INTEGER_LITERAL == VALUE_TYPE) || (BOOLEAN_VALUE == VALUE_TYPE) \
	 || (STRING_LITERAL == VALUE_TYPE))

// Initialize a stack-allocated ydb_buffer_t to point to a stack-allocated buffer (char [])
#define OCTO_SET_BUFFER(BUFFER, STRING)                                                                                     \
	{                                                                                                                   \
		BUFFER.buf_addr = STRING;                                                                                   \
		BUFFER.len_alloc = sizeof(STRING);                                                                          \
		BUFFER.len_used = 0; /* 0-initialize to indicate the buffer is empty in case an empty string is assigned */ \
	}

/* Initialize a stack-allocated ydb_buffer_t to point to a stack-allocated buffer (char []) with space for a null terminator
 * This is very similar to OCTO_SET_BUFFER except that it leaves space for 1 null terminator byte at the end.
 * To be used by callers which do a "ydb_get_s()" with "BUFFER" holding the result and then add a '\0' byte at the end
 * before doing further processing with functions like `strcmp()`, `strtol()` which require null terminated strings.
 */
#define OCTO_SET_NULL_TERMINATED_BUFFER(BUFFER, STRING)                                                                     \
	{                                                                                                                   \
		BUFFER.buf_addr = STRING;                                                                                   \
		BUFFER.len_alloc = sizeof(STRING) - 1;                                                                      \
		BUFFER.len_used = 0; /* 0-initialize to indicate the buffer is empty in case an empty string is assigned */ \
	}

// Free all resources associated with the Octo config file.
#define CLEANUP_CONFIG(CONFIG)          \
	{                               \
		config_destroy(CONFIG); \
		free(CONFIG);           \
	}

/* Below are special values used as part of the `aggregate_depth` parameter passed to `qualify_statement()`
 * to indicate we are qualifying a FROM clause, WHERE clause, GROUP BY clause etc.. These need to be negative as 0 is the
 * first valid depth used in the normal case (e.g. when qualifying the SELECT column list i.e. select->select_list) etc.
 * Some of these negative values indicate clauses where Aggregate functions are disallowed (i.e. FROM and WHERE).
 */
#define AGGREGATE_DEPTH_FROM_CLAUSE	-1
#define AGGREGATE_DEPTH_WHERE_CLAUSE	-2
#define AGGREGATE_DEPTH_GROUP_BY_CLAUSE -3

/* Timeouts used in various ydb_lock_incr_s() function calls */
#define TIMEOUT_1_SEC		  ((unsigned long long)1000000000)
#define TIMEOUT_5_SEC		  (5 * TIMEOUT_1_SEC)
#define TIMEOUT_DDL_EXCLUSIVELOCK (10 * TIMEOUT_1_SEC)

#define NEWLINE_NEEDED_FALSE FALSE
#define NEWLINE_NEEDED_TRUE  TRUE

/* Below macro initialize the query input buffer ("input_buffer_combined") */
#define INIT_INPUT_BUFFER                                                                    \
	{                                                                                    \
		/* Leave space for null terminator                                           \
		 * `read` takes the number of bytes to read _excluding_ the null terminator, \
		 * and we pass cur_input_max directly into read in `readline_get_more`.      \
		 */                                                                          \
		cur_input_max = INIT_QUERY_SIZE - 1;                                         \
		input_buffer_combined = calloc(1, INIT_QUERY_SIZE);                          \
		old_input_index = 0;                                                         \
		cur_input_index = 0;                                                         \
		cur_input_more = &no_more;                                                   \
		eof_hit = EOF_NONE;                                                          \
	}

/* Below macro copies a query QUERY with length QUERY_LENGTH into the query input buffer ("input_buffer_combined").
 * If NEWLINE_NEEDED is TRUE, then a '\n' and '\0' is added at end of buffer.
 * If NEWLINE_NEEDED is FALSE, then only a '\0' is added at end of buffer.
 */
#define COPY_QUERY_TO_INPUT_BUFFER(QUERY, QUERY_LENGTH, NEWLINE_NEEDED)                                            \
	{                                                                                                          \
		int padding;                                                                                       \
                                                                                                                   \
		assert((FALSE == NEWLINE_NEEDED) || (TRUE == NEWLINE_NEEDED));                                     \
		padding = NEWLINE_NEEDED + 1; /* 1 byte for '\n' if NEWLINE_NEEDED is TRUE and 1 byte for '\0'; */ \
		if (!NEWLINE_NEEDED) {                                                                             \
			cur_input_index = 0;                                                                       \
		}                                                                                                  \
		/* if query is too long to fit in the current buffer, resize buffer                                \
		 * by min(cur_input_max * 2, QUERY_LENGTH) + padding (for the \n\0)                                \
		 */                                                                                                \
		if (QUERY_LENGTH >= (cur_input_max - cur_input_index - padding)) {                                 \
			int   resize_amt;                                                                          \
			char *tmp;                                                                                 \
                                                                                                                   \
			resize_amt = ((QUERY_LENGTH > (cur_input_max * 2)) ? QUERY_LENGTH : (cur_input_max * 2));  \
			tmp = malloc(resize_amt + NEWLINE_NEEDED + 1);                                             \
			memcpy(tmp, input_buffer_combined, cur_input_index);                                       \
			free(input_buffer_combined);                                                               \
			input_buffer_combined = tmp;                                                               \
			cur_input_max = resize_amt;                                                                \
		}                                                                                                  \
		memcpy(&input_buffer_combined[cur_input_index], QUERY, QUERY_LENGTH);                              \
		if (NEWLINE_NEEDED) {                                                                              \
			input_buffer_combined[cur_input_index + QUERY_LENGTH] = '\n';                              \
		} else {                                                                                           \
			eof_hit = EOF_NONE;                                                                        \
			cur_input_more = &no_more;                                                                 \
		}                                                                                                  \
		input_buffer_combined[cur_input_index + QUERY_LENGTH + NEWLINE_NEEDED] = '\0';                     \
	}

/* Need to clear the entire input buffer. Not enough to just null terminate the first byte as the lexer operates
 * by reading 1 byte at a time. If the first byte is '\0', we will read input but once the input query is written
 * over the '\0', bytes after the input query will be read from the pre-existing contents of the buffer. Hence
 * the need to erase all contents in the buffer.
 */
#define ERASE_INPUT_BUFFER                                                                                          \
	{                                                                                                           \
		cur_input_index = 0;                                                                                \
		old_input_index = 0;                                                                                \
		/* See INIT_INPUT_BUFFER and COPY_QUERY_TO_INPUT_BUFFER macros for why the below assert is valid */ \
		assert((INIT_QUERY_SIZE - 1) <= cur_input_max);                                                     \
		memset(input_buffer_combined, 0, cur_input_max + 1);                                                \
	}

#define DELETE_QUERY_PARAMETER_CURSOR_LVN(CURSOR_YDB_BUFF)                                 \
	{                                                                                  \
		ydb_buffer_t cursor_local;                                                 \
		int	     dlqStatus;                                                    \
                                                                                           \
		YDB_STRING_TO_BUFFER(config->global_names.cursor, &cursor_local);          \
		dlqStatus = ydb_delete_s(&cursor_local, 1, CURSOR_YDB_BUFF, YDB_DEL_TREE); \
		YDB_ERROR_CHECK(dlqStatus);                                                \
	}

#define IS_STMT_BOOLEAN_AND(STMT) ((binary_STATEMENT == STMT->type) && (BOOLEAN_AND == STMT->v.binary->operation))

#ifndef NDEBUG
#define NDEBUG_ONLY(X)
#define DEBUG_ONLY(X) X
#else
#define NDEBUG_ONLY(X) X
#define DEBUG_ONLY(X)
#endif

// Convenience type definition for run_query callback function
typedef int (*callback_fnptr_t)(SqlStatement *, ydb_long_t, void *, char *, boolean_t);

int emit_column_specification(char **buffer, int *buffer_size, SqlColumn *cur_column);
int emit_create_table(FILE *output, struct SqlStatement *stmt);
int emit_create_function(FILE *output, struct SqlStatement *stmt);
// Recursively copies all of stmt, including making copies of strings

/**
 * Examines the table to make sure needed columns are specified, and fills out
 * any that are needed but not present.
 *
 * @returns 0 if success, 1 otherwise
 */
int create_table_defaults(SqlStatement *table_statement, SqlStatement *keywords_statement);

char *m_escape_string(const char *string);
int   m_escape_string2(char **buffer, int *buffer_len, char *string);
char *m_unescape_string(const char *string);

int	      readline_get_more();
SqlStatement *parse_line(ParseContext *parse_context);

int	      check_column_lists_for_type_match(SqlStatement *v); /* v->type is set_operation_STATEMENT or insert_STATEMENT */
int	      populate_data_type(SqlStatement *v, SqlValueType *type, ParseContext *parse_context);
SqlDataType   get_sqldatatype_from_sqlvaluetype(SqlValueType type);
SqlValueType  get_sqlvaluetype_from_sqldatatype(SqlDataType type, boolean_t is_unknown_type_okay);
SqlValueType  get_sqlvaluetype_from_psql_type(PSQL_TypeOid type);
PSQL_TypeOid  get_psql_type_from_sqlvaluetype(SqlValueType type);
PSQL_TypeSize get_type_size_from_psql_type(PSQL_TypeOid type);
SqlTable *    find_table(const char *table_name);
SqlFunction * find_function(const char *function_name, const char *function_hash);
void	      get_function_name_and_parmtypes(char *ret_buff, int ret_buff_len, char *function_name, SqlStatement *parm_list_stmt);
int	   drop_schema_from_local_cache(ydb_buffer_t *name_buffer, SqlSchemaType schema_type, ydb_buffer_t *function_hash_buffer);
SqlColumn *find_column(char *column_name, SqlTable *table);
SqlStatement *find_column_alias_name(SqlStatement *stmt);
void	      parse_tree_optimize(SqlSelectStatement *select);
void	      move_where_clause_to_on_clause(SqlStatement **stmt_ptr, SqlJoin *start_join);
SqlStatement *traverse_where_clause(SqlStatement *binary_stmt, SqlJoin *start_join);
int	      qualify_query(SqlStatement *table_alias_stmt, SqlJoin *parent_join, SqlTableAlias *parent_table_alias,
			    QualifyStatementParms *ret);
int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *table_alias_stmt, int depth, QualifyStatementParms *ret);
SqlColumnAlias *    qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *table_alias_stmt, int depth,
					SqlColumnListAlias **ret_cla);
SqlColumnListAlias *match_column_in_table(SqlTableAlias *table, char *column_name, int column_name_len, boolean_t *ambiguous,
					  boolean_t issue_error);
boolean_t	    match_column_list_alias_in_select_column_list(SqlColumnListAlias *match_cla, SqlStatement *cla_stmt);
SqlOptionalKeyword *get_keyword(SqlColumn *column, enum OptionalKeyword keyword);
SqlOptionalKeyword *get_keyword_from_keywords(SqlOptionalKeyword *start_keyword, enum OptionalKeyword keyword);
int		    get_key_columns(SqlTable *table, SqlColumn **key_columns);
int  generate_key_name(char **buffer, int *buffer_size, int target_key_num, SqlTable *table, SqlColumn **key_columns);
int  get_row_count_from_plan_name(char *plan_name, ydb_long_t cursorId);
int  print_temporary_table(SqlStatement *, ydb_long_t cursorId, void *parms, char *plan_name, boolean_t send_row_description);
void print_result_row(ydb_buffer_t *row);
int  get_mval_len(unsigned char *buff, int *data_len);

/**
 * Parses query, and calls the callback if it is a select statement. Otherwise, the query is a data altering
 *  query and gets executed
 *
 * @returns TRUE on success, FALSE on failure
 */
int run_query(callback_fnptr_t callback, void *parms, boolean_t send_row_description, ParseContext *parse_context);

char *get_aggregate_func_name(SqlAggregateType type);
char *get_set_operation_string(SqlSetOperationType type);
char *get_user_visible_type_string(SqlValueType type);

/* Hashing support functions */
int  generate_routine_name(hash128_state_t *state, char *routine_name, int routine_len, FileType file_type);
void hash_canonical_query(hash128_state_t *state, SqlStatement *stmt, int *status);
void ydb_hash_to_string(ydb_uint16 *hash, char *buffer, const unsigned int buf_len);

void		    assign_table_to_columns(SqlStatement *table_statement);
SqlOptionalKeyword *add_optional_piece_keyword_to_sql_column(int column_number);

// Converts a list of columns to a column list alias associated with the given table alias
SqlColumnListAlias *columns_to_column_list_alias(SqlColumn *column, SqlStatement *table_alias_stmt);

SqlStatement *	    drill_to_table_alias(SqlStatement *sqlStmt);
int		    get_column_piece_number(SqlColumnAlias *column_alias, SqlTableAlias *table_alias);
int		    get_column_number_from_column_list_alias(SqlColumnListAlias *input_cla, SqlTableAlias *table_alias);
SqlColumnListAlias *get_column_list_alias_n_from_table_alias(SqlTableAlias *table_alias, int column_number);
SqlColumnAlias *    get_column_alias_for_column_list_alias(SqlColumnListAlias *col_cla, SqlStatement *matching_alias_stmt);

SqlColumnListAlias *copy_column_list_alias_list(SqlColumnListAlias *cla, SqlStatement *sql_stmt, SqlStatement *keywords);
SqlStatement *	    copy_sql_statement(SqlStatement *stmt);
boolean_t	    match_sql_statement(SqlStatement *stmt, SqlStatement *match_stmt);

void	      compress_statement(SqlStatement *stmt, char **out, int *out_length);
SqlStatement *decompress_statement(char *buffer, int out_length);

int store_table_definition(ydb_buffer_t *table_name_buff, char *table_defn, int table_defn_length, boolean_t is_text);
int store_function_definition(ydb_buffer_t *function_name_buffers, char *function_defn, int function_defn_length,
			      boolean_t is_text);

int  store_table_in_pg_class(SqlTable *table, ydb_buffer_t *table_name_buffer);
int  delete_table_from_pg_class(ydb_buffer_t *table_name_buffer);
void cleanup_tables();
int  store_function_in_pg_proc(SqlFunction *function, char *function_hash);
int  delete_function_from_pg_proc(ydb_buffer_t *function_name_buffer, ydb_buffer_t *function_hash_buffer);
int  regex_has_no_special_characters(SqlStatement *op1, enum RegexType regex_type, ParseContext *parse_context);

/* Parse related functions invoked from the .y files (parser.y, select.y etc.) */
void	      as_name(SqlStatement *as_name);
SqlStatement *sql_set_statement(SqlStatement *variable, SqlStatement *value, ParseContext *parse_context);
SqlStatement *aggregate_function(SqlAggregateType aggregate_type, OptionalKeyword set_quantifier, SqlStatement *value_expression);
SqlStatement *between_predicate(SqlStatement *row_value_constructor, SqlStatement *from, SqlStatement *to, boolean_t not_specified);
SqlStatement *cast_specification(SqlStatement *cast_specification, SqlStatement *source);
SqlStatement *create_sql_column_list(SqlStatement *elem, SqlStatement *tail, YYLTYPE *llocp);
int	      copy_correlation_specification_aliases(SqlTableAlias *table_alias, SqlStatement *correlation_specification);
SqlStatement *data_type(SqlDataType data_type, SqlStatement *size_or_precision, SqlStatement *scale);
SqlStatement *derived_column(SqlStatement *derived_column_expression, SqlStatement *column_name, struct YYLTYPE *yloc);
SqlStatement *derived_table(SqlStatement *table_subquery, SqlStatement *correlation_specification);
SqlStatement *grouping_column_reference(SqlStatement *derived_column_expression, SqlStatement *collate_clause);
SqlStatement *insert_statement(SqlStatement *table_name, SqlStatement *column_name_list, SqlStatement *query_expression,
			       int *plan_id, ParseContext *parse_context);
int	      natural_join_condition(SqlJoin *start, SqlJoin *r_join);
int	      parse_literal_to_parameter(ParseContext *parse_context, SqlValue *value, boolean_t update_existing);
SqlStatement *query_specification(OptionalKeyword set_quantifier, SqlStatement *select_list, SqlStatement *table_expression,
				  SqlStatement *sort_specification_list, int *plan_id);
SqlStatement *validate_query_expression(SqlStatement *query_expression, ParseContext *parse_context, SqlStatementType cmd_type);
int regex_specification(SqlStatement **stmt, SqlStatement *op0, SqlStatement *op1, enum RegexType regex_type, int is_sensitive,
			int is_not, ParseContext *parse_context);
SqlStatement *set_operation(enum SqlSetOperationType setoper_type, SqlStatement *left_operand, SqlStatement *right_operand);
SqlStatement *sort_specification(SqlStatement *sort_key, SqlStatement *ordering_specification);
SqlStatement *table_expression(SqlStatement *from, SqlStatement *where, SqlStatement *group_by, SqlStatement *having);
SqlStatement *table_reference(SqlStatement *column_name, SqlStatement *correlation_specification, int *plan_id);

// Creates a new cursor by assigning a new cursorId
int64_t	  create_cursor(ydb_buffer_t *schema_global, ydb_buffer_t *cursor_buffer);
boolean_t is_query_canceled(callback_fnptr_t callback, int32_t cursorId, void *parms, char *plan_name,
			    boolean_t send_row_description);

/**
 * Returns TRUE if the columns are equal, FALSE otherwise
 */
int columns_equal(SqlColumn *a, SqlColumn *b);
int tables_equal(SqlTable *a, SqlTable *b);
int values_equal(SqlValue *a, SqlValue *b);

// Returns the amount of memory used by the current process
int64_t get_mem_usage();

int no_more();

int  get_input(char *buf, int size);
void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, ParseContext *parse_context, char const *s);

int get_full_path_of_generated_m_file(char *filename, int filename_len, char *m_routine_name);
int get_full_path_of_generated_o_file(char *filename, int filename_len, char *o_routine_name);

int auto_load_octo_seed(void);
int auto_load_octo_seed_if_needed(void);
int auto_upgrade_plan_definition_if_needed(void);
int auto_upgrade_binary_definition_if_needed(void);
int auto_upgrade_binary_function_definition(void);
int auto_upgrade_binary_table_definition(void);

/* Globals */
extern uint64_t hash_canonical_query_cycle; // incremented before every outermost call to "hash_canonical_query"
extern int	cur_input_index;	    // Current index of input_buffer_combined the parser should read from,
					    // and readlines should write to. Effectively marks the end of the
					    // current query.
extern int old_input_index;		    // The previous value of cur_input_index before the parser modifies it.
					    // Effectively marks the start of the current query.
extern int leading_spaces;		    // leading spaces in the current query it needs to be stored somewhere
					    // accessible but should be ignored, except by the lexer and yyerror
extern int   cur_input_max;
extern int   eof_hit;
extern FILE *inputFile;
extern char *input_buffer_combined; // The input buffer for octo. Contains the query strings.
extern int (*cur_input_more)();
extern OctoConfig * config;
extern ydb_buffer_t lex_buffer;		// String buffer for use in lexer.l
extern int	    ydb_release_number; /* e.g. the integer 130 in case of r1.30 etc. */

#endif
