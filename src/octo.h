/****************************************************************
 *								*
 * Copyright (c) 2019-2025 YottaDB LLC and/or its subsidiaries.	*
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
#include <unistd.h>    /* for unlink() */

#include <libyottadb.h>

#include "rocto/message_formats.h"
#include "errors.h"
#include "octo_types.h"
#include "config.h"
#include "constants.h"
#include "memory_chunk.h"
#include "pg_defaults.h" /* Only used for DEFAULT_DATESTYLE */

#include "mmrhash.h"

/* Below is a macro that converts a numeric argument to a string literal using the C preprocessor. */
#define STRINGIZE(X) #X

/* Below is a macro that converts a numeric argument (that is in turn defined as a macro) to a string literal
 * using the C preprocessor. See https://gcc.gnu.org/onlinedocs/gcc-8.4.0/cpp/Stringizing.html for details on below macros.
 */
#define MACRO_STRINGIZE(X) STRINGIZE(X)

// Max number of chars needed to convert signed or unsigned 8-bit int to char*, including null terminator and possible negative sign
#define INT8_TO_STRING_MAX 5
// Max number of chars needed to convert signed or unsigned 16-bit int to char*, including null terminator and possible negative
// sign
#define INT16_TO_STRING_MAX 7
// Max number of chars needed for length of 1048576 (max number of pieces in YottaDB) + null terminator + possible negative sign
#define INT20_TO_STRING_MAX 9
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
#define MAX_KEY_COUNT 256
// Maximum length for database emulation strings, e.g. "PostgreSQL"
#define MAX_EMULATION_STRING_LEN 12
// Length of M extrinsic function prefix, i.e. "$$"
#define EXTRINSIC_PREFIX_LEN 2

#define IS_KEY_COLUMN(COLUMN)		      (NULL != get_keyword(COLUMN, OPTIONAL_KEY_NUM))
#define IS_COLUMN_NOT_NULL(COLUMN)	      (IS_KEY_COLUMN(COLUMN) || IS_COLUMN_IDENTITY(COLUMN) || (NULL != get_keyword(COLUMN, NOT_NULL)))
#define IS_COLUMN_IDENTITY(COLUMN)	      ((IS_COLUMN_ALWAYS_IDENTITY(COLUMN) || (IS_COLUMN_BY_DEFAULT_IDENTITY(COLUMN))))
#define IS_COLUMN_ALWAYS_IDENTITY(COLUMN)     (NULL != get_keyword(COLUMN, OPTIONAL_GENERATED_ALWAYS_IDENTITY))
#define IS_COLUMN_BY_DEFAULT_IDENTITY(COLUMN) (NULL != get_keyword(COLUMN, OPTIONAL_GENERATED_BY_DEFAULT_IDENTITY))
#define IS_COLUMN_STRING_TYPE(COLUMN)	      (STRING_TYPE == COLUMN->data_type_struct.data_type)

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

/* History defines used in history.c */
#define OCTO_HISTORY_DEFAULT		"~/.octo_history"
#define OCTO_HISTORY_MAX_LENGTH_DEFAULT 500
#define OCTO_HISTORY_MAX_LENGTH_UNSET	-1

// Allows us to leave parameters in place even if they are unused and avoid warning from the
// compiler.
#define UNUSED(x) (void)(x)

/* Set maximum trigger name length
 * This cannot be greater than MAX_USER_TRIGNAME_LEN defined in https://gitlab.com/YottaDB/DB/YDB/blob/master/sr_unix/trigger.h
 */
#define MAX_TRIGGER_LEN 28

// Specify character for delimiting column values in Octo
#define COLUMN_DELIMITER "|"
#define EMPTY_DELIMITER	 ""

#define VALUES_COLUMN_NAME_PREFIX "column"

#define ZERO_OUTPUT_KEY_ID 0

// Sql Standard format
#define DEFAULT_DATE_FORMAT		  "%m-%d-%Y"
#define DEFAULT_TIME_FORMAT		  "%H:%M:%S"
#define DEFAULT_TIMETZ_FORMAT		  "%H:%M:%S%z"
#define DEFAULT_TIMESTAMP_FORMAT	  "%m-%d-%Y %H:%M:%S"
#define POSTGRES_DATE_FORMAT		  "%Y-%m-%d"
#define POSTGRES_TIME_FORMAT		  DEFAULT_TIME_FORMAT
#define POSTGRES_TIMETZ_FORMAT		  DEFAULT_TIMETZ_FORMAT
#define POSTGRES_TIMESTAMP_FORMAT	  "%Y-%m-%d %H:%M:%S"
#define POSTGRES_TIMESTAMPTZ_FORMAT	  "%Y-%m-%d %H:%M:%S%z"
#define MYSQL_DATE_FORMAT		  "%Y-%m-%d"
#define MYSQL_TIME_FORMAT		  DEFAULT_TIME_FORMAT
#define MYSQL_TIMESTAMP_FORMAT		  "%Y-%m-%d %H:%M:%S"
#define DEFAULT_OUTPUT_DATE_FORMAT	  "%Y-%m-%d"
#define DEFAULT_OUTPUT_TIME_FORMAT	  DEFAULT_TIME_FORMAT
#define DEFAULT_OUTPUT_TIMETZ_FORMAT	  DEFAULT_TIMETZ_FORMAT
#define DEFAULT_OUTPUT_TIMESTAMP_FORMAT	  "%Y-%m-%d %H:%M:%S"
#define DEFAULT_OUTPUT_TIMESTAMPTZ_FORMAT "%Y-%m-%d %H:%M:%S%z"
#define OCTO_DEFAULT_MYSQL_DATESTYLE	  OCTO_DEFAULT_DATESTYLE
// Set YMD as the default as we would like the result and input format to match
// Postgres supports YMD so this will not cause any issues
#define OCTO_DEFAULT_POSTGRES_DATESTYLE	    OCTO_DEFAULT_DATESTYLE
#define OCTO_DEFAULT_DATESTYLE		    "ISO, YMD"
#define MAX_DATE_TIME_INTERNAL_FORMAT_VALUE 253402300799999999
#define MIN_DATE_TIME_INTERNAL_FORMAT_VALUE -62167219200999999
#define DATE_TIME_ERROR_RETURN		    MAX_DATE_TIME_INTERNAL_FORMAT_VALUE + 1

/* Define various string literals used as gvn/lvn subscripts and/or in physical plans (generated M code).
 * Each macro is prefixed with a OCTOLIT_.
 * A similar piece of code exists in template_helpers.h where PP_* macros are defined.
 */
#define OCTOLIT_KEYS	    "keys"	   /* keep this in sync with PP_KEYS in "template_helpers.h" */
#define OCTOLIT_PARAMETERS  "parameters"   /* keep this in sync with PP_PARAMETERS in "template_helpers.h" */
#define OCTOLIT_ROW_COUNT   "RowCount"	   /* keep this in sync with PP_ROW_COUNT in "template_helpers.h" */
#define OCTOLIT_XREF_STATUS "xref_status"  /* keep this in sync with PP_XREF_STATUS in "template_helpers.h" */
#define OCTO_LEFT_JOIN_LIT  "octoLeftJoin" /* keep this in sync with PP_OCTO_LEFT_JOIN in "template_helpers.h" */

#define OCTOLIT_0			    "0"
#define OCTOLIT_ALL			    "all"
#define OCTOLIT_BINARY			    "binary"
#define OCTOLIT_BINFMT			    "binfmt"
#define OCTOLIT_BOUND			    "bound"
#define OCTOLIT_CHECK			    "check"
#define OCTOLIT_CHUNK			    "chunk"
#define OCTOLIT_COLUMN			    "column"
#define OCTOLIT_COLUMNS			    "columns"
#define OCTOLIT_COLUMN_ID		    "column_id"
#define OCTOLIT_CONSTRAINT		    "constraint"
#define OCTOLIT_CHECK_CONSTRAINT	    "check_constraint"
#define OCTOLIT_EXTRACTFUNCTION		    "extractfunction"
#define OCTOLIT_DATA_TYPE		    "data_type"
#define OCTOLIT_DATA_TYPE_SIZE		    "data_type_size"
#define OCTOLIT_DDL			    "ddl"
#define OCTOLIT_FILES			    "files"
#define OCTOLIT_FROMVIEW		    "fromview"
#define OCTOLIT_FUNCTIONVIEWDEPENDENCY	    "functionviewdependency"
#define OCTOLIT_FUNCTIONS		    "functions"
#define OCTOLIT_FUNCTIONS_MAP		    "functions_map"
#define OCTOLIT_FORMAT_CODE		    "format_code"
#define OCTOLIT_IDENTITY		    "identity"
#define OCTOLIT_KEY			    "key"
#define OCTOLIT_PKEY			    "pkey"
#define OCTOLIT_LENGTH			    "length"
#define OCTOLIT_TEXT_LENGTH		    "text_length"
#define OCTOLIT_MYSQL			    "MySQL"
#define OCTOLIT_NAME			    "name"
#define OCTOLIT_NONE			    "none"
#define OCTOLIT_OID			    "oid"
#define OCTOLIT_OCTOONEROWTABLE		    "octoonerowtable"
#define OCTOLIT_OUTPUT_COLUMNS		    "output_columns"
#define OCTOLIT_OUTPUT_KEY		    "output_key"
#define OCTOLIT_PERMISSIONS		    "permissions"
#define OCTOLIT_PG_ATTRIBUTE		    "pg_attribute"
#define OCTOLIT_PG_CATALOG		    "pg_catalog"
#define OCTOLIT_PG_CLASS		    "pg_class"
#define OCTOLIT_PG_SETTINGS		    "pg_settings"
#define OCTOLIT_PLANDIRS		    "plandirs"
#define OCTOLIT_PLANFMT			    "planfmt"
#define OCTOLIT_PLAN_METADATA		    "plan_metadata"
#define OCTOLIT_POSTGRESQL		    "PostgreSQL"
#define OCTOLIT_PREPARED		    "prepared"
#define OCTOLIT_PRIMARY_KEY_NAME	    "primary_key_name"
#define OCTOLIT_READ_ONLY		    "read-only"
#define OCTOLIT_ROUTINE			    "routine"
#define OCTOLIT_SEEDDFNFMT		    "seeddfnfmt"
#define OCTOLIT_SETTINGS		    "settings"
#define OCTOLIT_T			    "t"
#define OCTOLIT_TABLEPLANS		    "tableplans"
#define OCTOLIT_TABLEVIEWDEPENDENCY	    "tableviewdependency"
#define OCTOLIT_TABLES			    "tables"
#define OCTOLIT_TABLE			    "TABLE"
#define OCTOLIT_TABLE_ID		    "table_id"
#define OCTOLIT_TABLECONSTRAINT		    "tableconstraint"
#define OCTOLIT_TAG			    "tag"
#define OCTOLIT_TEXT			    "text"
#define OCTOLIT_TIMESTAMP		    "timestamp"
#define OCTOLIT_TYPE_MODIFIER		    "type_modifier"
#define OCTOLIT_USER			    "user"
#define OCTOLIT_USERS			    "users"
#define OCTOLIT_VALUE			    "value"
#define OCTOLIT_VALUES			    "values"
#define OCTOLIT_VARIABLE		    "variable"
#define OCTOLIT_VIEWS			    "views"
#define OCTOLIT_VIEW			    "VIEW"
#define OCTOLIT_VIEWPLANS		    "viewplans"
#define OCTOLIT_VIEWDEPENDENCY		    "viewdependency"
#define OCTOLIT_YDBOCTO			    "^%ydbocto"
#define OCTOLIT_YDBOCTOCANCEL		    "%ydboctoCancel"
#define OCTOLIT_YDBOCTOCANCELLOCALXF	    "localTableXref"
#define OCTOLIT_YDBOCTOSECRETKEYLIST	    "%ydboctoSecretKeyList"
#define OCTOLIT_YDBOCTOTBLCONSTRAINT	    "%ydboctoTblConstraint"
#define OCTOLIT_YDBOCTOSCHEMA		    "^%ydboctoschema"
#define OCTOLIT_YDBOCTOTBLEXTRACT	    "%ydboctoTblExtract"
#define OCTOLIT_YDBOCTOVIEWSORTEDNAMES	    "%viewssortednames"
#define OCTOLIT_YDBOCTOVIEWCREATED	    "%ydboctoviewcreated"
#define OCTOLIT_YDBOCTODATETIMEOUTPUTFORMAT "%ydboctodatetimeoutputformat"
#define OCTOLIT_DATE_TIME_HOROLOG	    "horolog"
#define OCTOLIT_DATE_TIME_ZHOROLOG	    "zhorolog"
#define OCTOLIT_DATE_TIME_ZUT		    "zut"
#define OCTOLIT_DATE_TIME_FILEMAN	    "fileman"
#define OCTOLIT_DATE_TIME_TEXT		    "text"

#define OCTOLIT_YDBOCTOVIEWDEPENDENCY "%ydboctoViewDependency"
#define OCTOLIT_AIM_OCTO_CACHE	      "^%ydbAIMOctoCache"
#define OCTOLIT_AIM_SUB_COMPLETED     "completed?"
#define OCTOLIT_AIM_SUB_LOCATION      "location"
#define OCTOLIT_AIM_SUB_CANCELXREF    "aimXref"
#define OCTOLIT_XC_PATH		      "xc_path"

/* YDBOcto#929 related macros */
#define OCTOLIT_YDBOCTO929 "%ydbocto929"

#define OCTO929_SQLFILE_NAME "/tmp/ydbocto929_auto_upgrade_XXXXXX"

#define OCTOLIT_SEED_OBJECT_LIST "^%ydboctoseed"
/* Macros for StartupMessage parameters sent by client that are NOT actual runtime parameters.
 * These strings are used for selective exclusion of such parameters via strncmp checks in rocto.c
 * to prevent ERR_INVALID_RUNTIME_PARAMETER errors at remote connection startup.
 */
#define OCTOLIT_USER_LOWER     "user"
#define OCTOLIT_DATABASE_LOWER "database"
/* Macros for read-only runtime parameters. Used for conditionally issuing ERR_PARM_CANNOT_BE_CHANGED
 * message in set_parameter_in_pg_settings.c.
 */
#define OCTOLIT_IS_SUPERUSER_LOWER	    "is_superuser"
#define OCTOLIT_SESSION_AUTHORIZATION_LOWER "session_authorization"

// Macros for plan size allocation in physical plan generation
#define OCTOPLAN_LIT	  "octoPlan"
#define XREFPLAN_LIT	  "xrefPlan"
#define MAX_PLAN_NAME_LEN sizeof(OCTOPLAN_LIT) + INT32_TO_STRING_MAX

/* Macros for various prefixes. All of them having the same length. */
#define TABLE_GLOBAL_NAME_PREFIX  "%ydboctoD" /* corresponds to NameType = "TableGlobal"    in octo_types.h */
#define FUNCTION_NAME_PREFIX	  "%ydboctoF" /* corresponds to NameType = "FunctionHash"   in octo_types.h */
#define PHYSICAL_PLAN_NAME_PREFIX "%ydboctoP" /* corresponds to NameType = "OutputPlan"     in octo_types.h */
#define UNIQUE_GLOBAL_NAME_PREFIX "%ydboctoU" /* corresponds to NameType = "UniqueGlobal"   in octo_types.h */
#define XREF_PLAN_NAME_PREFIX	  "%ydboctoX" /* corresponds to NameType = "CrossReference" in octo_types.h */

/* Below macro defines the name of the hidden primary key column that is added by Octo in a READWRITE table
 * which has no primary key columns specified by the user in the CREATE TABLE command. For a READONLY table
 * all columns specified by the user are together assumed to be the primary key.
 * All column names that are Octo created have a %YO prefix (short form for YdbOcto).
 */
#define HIDDEN_KEY_COL_NAME "%yo_keycol"

/* Below defines the default values used for all row description messages currently sent by Rocto */
#define ROWDESC_DEFAULT_TYPE_MODIFIER -1
#define ROWDESC_DEFAULT_FORMAT_CODE   0

/* Set maximum command tag length for use in extended query protocol, including null terminator
 * This value should be large enough to hold the longest possible first keyword of a SQL query, i.e. "DEALLOCATE" i.e. 10 bytes
 * In addition, in case of a "SELECT", the tag would be followed by a space and a 4-byte integer count (number of rows returned).
 * And in case of a "INSERT", the tag would be followed by 2 spaces and 2 4-byte integers so account for those.
 * In the case of a "DELETE", the tag would be followed by 1 space and 1 4-byte integer so whatever space calculations
 * were done for "INSERT" is guaranteed to be more than enough for "DELETE".
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

/* For DELETE FROM, Octo conforms to Postgres output format (see https://www.postgresql.org/docs/9.5/sql-delete.html for details).
 * Relevant part pasted below.
 * ---------------------------------------------------------------------------------
 * On successful completion, a DELETE command returns a command tag of the form
 * DELETE count
 * The count is the number of rows deleted. Note that the number may be less than the number
 * of rows that matched the condition when deletes were suppressed by a BEFORE DELETE trigger.
 * If count is 0, no rows were deleted by the query (this is not considered an error).
 * ---------------------------------------------------------------------------------
 */
#define DELETE_COMMAND_TAG "DELETE"

/* Like INSERT and DELETE above, Octo conforms to Postgres output format for UPDATE too */
#define UPDATE_COMMAND_TAG "UPDATE"

#define SET_COMMAND_TAG		    "SET"
#define SHOW_COMMAND_TAG	    "SHOW"
#define CREATE_TABLE_COMMAND_TAG    "CREATE TABLE"
#define DROP_TABLE_COMMAND_TAG	    "DROP TABLE"
#define CREATE_VIEW_COMMAND_TAG	    "CREATE VIEW"
#define DROP_VIEW_COMMAND_TAG	    "DROP VIEW"
#define CREATE_FUNCTION_COMMAND_TAG "CREATE FUNCTION"
#define DROP_FUNCTION_COMMAND_TAG   "DROP FUNCTION"
#define TRUNCATE_TABLE_COMMAND_TAG  "TRUNCATE TABLE"
#define BEGIN_COMMAND_TAG	    "BEGIN"
#define COMMIT_COMMAND_TAG	    "COMMIT"
#define ROLLBACK_COMMAND_TAG	    "ROLLBACK"

#define PRINT_COMMAND_TAG(COMMAND_TAG)                                                                                     \
	/* Skip printing COMMAND TAG if running auto load of octo-seed.sql as it is internal (not a user driven activity). \
	 * Skip printing COMMAND TAG if rocto as that should not go to stdout (should only go to client through simple     \
	 * or extended query protocol connection.                                                                          \
	 */                                                                                                                \
	if (!config->in_auto_load_octo_seed && !config->is_auto_upgrade_octo929 && !config->is_rocto) {                    \
		fprintf(stdout, "%s\n", COMMAND_TAG);                                                                      \
		fflush(stdout);                                                                                            \
	}

// Default buffer allocated for $zroutines
#define ZRO_INIT_ALLOC 512

/* Maximum size of each fragment a table or function binary or text definition is split into in M nodes */
#define MAX_DEFINITION_FRAGMENT_SIZE 32768

#define YDB_MAX_KEY_SZ 1023 /* 1023 can be replaced by MAX_STR_LEN (from YDB repo) if it is exposed in libyottadb.h */

#define YDB_MAX_INT_VAL                                                                                         \
	"999999999999999999" /* Largest integer in YDB without losing precision (18 digits). Doing this integer \
			      * + 1 would give us a number that will no longer return precise results when one  \
			      * does a - 1 on it. This integer value is currently not defined as a macro in the \
			      * YDB repository but is used in YDB/sr_unix/ygblstat.mpt.                         \
			      */

/* Size of query buffer initially allocated. Gets expanded as need arises. */
#define INIT_QUERY_SIZE 32768

/* The below macro needs to be manually bumped if binary table OR function definition format changes due to
 * dependent structure layout changes (e.g. SqlStatement structure etc.).
 * The "test-auto-upgrade" pipeline job (that automatically runs) will alert us if it detects the need for the bump.
 * And that is considered good enough for now (i.e. no manual review of code necessary to detect the need for a bump).
 */
#define FMT_BINARY_DEFINITION 20

/* The below macro needs to be manually bumped if at least one of the following changes.
 *	1) Generated physical plan (_ydboctoP*.m) file name OR contents
 *	2) Generated cross-reference/xref plan (_ydboctoX*.m) file name OR contents (if any)
 *	3) Generated trigger (associated with a cross-reference/xref plan) name OR contents (if any)
 * The "test-auto-upgrade" pipeline job (that automatically runs) will alert us if it detects the need for the bump.
 * And that is considered good enough for now (i.e. no manual review of code necessary to detect the need for a bump).
 */
#define FMT_PLAN_DEFINITION 36

/* The below macro needs to be manually bumped if there is a non-cosmetic change to octo-seed.sql or code/gvn change that helps
 * octo-seed.sql objects not to be dropped (src/ensure_seed_objects_are_not_dropped.c)
 */
#define FMT_SEED_DEFINITION 10

#define FMT_SEED_DEFINITION_OCTO929                                                 \
	7 /* The value of FMT_SEED_DEFINITION when YDBOcto#929 changes were merged. \
	   * This requires a special auto upgrade (upper case names to lower case)  \
	   * hence the special macro for this case.                                 \
	   */

/* Used by `hash_canonical_query()` */
#define HASH_LITERAL_VALUES -1

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

/* Convert a string to lowercase and store in provided destination.
 * Note that DEST and START may or may not be the same. This macro handles both cases.
 */
#define TOLOWER(DEST, DEST_END, START, END)                                                 \
	{                                                                                   \
		char *sTART;                                                                \
                                                                                            \
		sTART = (START);                                                            \
		while (sTART < END) {                                                       \
			(*DEST) = tolower(*sTART);                                          \
			(DEST)++;                                                           \
			sTART++;                                                            \
		}                                                                           \
		(*DEST) = '\0';                                                             \
		(DEST)++;                                                                   \
		/* Check for buffer overflow. It is okay if DEST < DEST_END as there are */ \
		/* cases where this macro is applied piecemeal to a single buffer. */       \
		assert(DEST <= DEST_END);                                                   \
		UNUSED(DEST_END); /* avoids [-Wunused-but-set-variable] compiler warning */ \
	}

// Convert a string to lowercase in place
#define TOLOWER_STR(STR)                             \
	{                                            \
		size_t len;                          \
		char  *end, *begin;                  \
                                                     \
		begin = STR;                         \
		len = strlen(begin);                 \
		end = begin + len;                   \
		TOLOWER(begin, end + 1, begin, end); \
	}

#define TOLOWER_SUBSTR(STR, LEN)                                                          \
	{                                                                                 \
		char *end, *begin;                                                        \
                                                                                          \
		begin = STR;                                                              \
		end = begin + LEN;                                                        \
		while (begin < end) {                                                     \
			(*begin) = tolower(*begin);                                       \
			begin++;                                                          \
		}                                                                         \
		/* Check for buffer overflow. It is okay if (begin < end) as there are */ \
		/* cases where this macro is applied piecemeal to a single buffer. */     \
		assert(begin <= end);                                                     \
	}

/* Convert a string to uppercase and store in provided destination
 * Note that DEST and START may or may not be the same. This macro handles both cases.
 */
#define TOUPPER(DEST, DEST_END, START, END)                                                                             \
	{                                                                                                               \
		char *sTART;                                                                                            \
                                                                                                                        \
		sTART = (START);                                                                                        \
		while (sTART < END) {                                                                                   \
			(*DEST) = toupper(*sTART);                                                                      \
			(DEST)++;                                                                                       \
			sTART++;                                                                                        \
		}                                                                                                       \
		(*DEST) = '\0';                                                                                         \
		(DEST)++;                                                                                               \
		assert(DEST <= DEST_END);                                                                               \
		UNUSED(DEST_END); /* needed in some cases to avoid [-Wunused-but-set-variable] warning from compiler */ \
	}

// Convert a string to uppercase in place
#define TOUPPER_STR(STR)                             \
	{                                            \
		size_t len;                          \
		char  *end;                          \
		char  *begin;                        \
                                                     \
		begin = STR;                         \
		len = strlen(begin);                 \
		end = begin + len;                   \
		TOUPPER(begin, end + 1, begin, end); \
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
#define DOUBLE_ARRAY_ALLOCATION(ARRAY, SIZE, TYPE, MAX)                        \
	{                                                                      \
		int new_size;                                                  \
                                                                               \
		new_size = SIZE * 2;                                           \
		if ((0 < MAX) && (MAX < new_size)) {                           \
			new_size = MAX;                                        \
		}                                                              \
		TYPE *new_array;                                               \
		new_array = (TYPE *)malloc(new_size * sizeof(TYPE));           \
		memcpy(new_array, ARRAY, SIZE * sizeof(TYPE));                 \
		memset(&new_array[SIZE], 0, (new_size - SIZE) * sizeof(TYPE)); \
		free(ARRAY);                                                   \
		ARRAY = new_array;                                             \
		SIZE = new_size;                                               \
	}

/* Increase allocated size of a ydb_buffer_t. Assumes len_used is set to the desired new size as is done
 * when YDB_ERR_INVSTRLEN is encountered.
 */
#define EXPAND_YDB_BUFFER_T_ALLOCATION(BUFFER)                        \
	{                                                             \
		int newsize = BUFFER.len_used;                        \
                                                                      \
		YDB_FREE_BUFFER(&BUFFER);                             \
		OCTO_MALLOC_NULL_TERMINATED_BUFFER(&BUFFER, newsize); \
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

typedef enum ExpressionMatchType {
	MatchExpressionOFlow,
	NoMatchExpression,
	KeysExpression,
	ValuesExpression,
} ExpressionMatchType;

typedef enum DDLDependencyType {
	DDL_CheckConstraint,
	DDL_ExtractFunction,
} DDLDependencyType;

#define INVOKE_QUERY_SPECIFICATION(Q_SPEC, SET_QT, SELECT_LIST, TABLE_EXPR, SORT_SPEC_LIST, PLAN_ID)    \
	{                                                                                               \
		Q_SPEC = query_specification(SET_QT, SELECT_LIST, TABLE_EXPR, SORT_SPEC_LIST, PLAN_ID); \
		if (NULL == Q_SPEC)                                                                     \
			YYABORT;                                                                        \
	}

#define INVOKE_REGEX_SPECIFICATION(STMT, OP0, OP1, IS_REGEX_LIKE_OR_SIMILAR, IS_SENSITIVE, IS_NOT)            \
	{                                                                                                     \
		int status;                                                                                   \
                                                                                                              \
		status = regex_specification(STMT, OP0, OP1, IS_REGEX_LIKE_OR_SIMILAR, IS_SENSITIVE, IS_NOT); \
		if (0 != status)                                                                              \
			YYABORT;                                                                              \
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
	 || (STRING_LITERAL == VALUE_TYPE) || (IS_DATE_TIME_TYPE(VALUE_TYPE)))

#define IS_NULL_FIXED_VALUE(FIX_VALUE) \
	((NULL != FIX_VALUE) && (LP_VALUE == FIX_VALUE->type) && (IS_NULL_LITERAL == FIX_VALUE->v.lp_value.value->type))

#define IS_NUL_VALUE(TYPE) (NUL_VALUE == TYPE)

/* A NULL is also considered compatible with BOOLEAN, STRING, INTEGER, NUMERIC etc.
 * A 't'/'f' literal is considered compatible with BOOLEAN and STRING.
 * Below macros take that into account.
 */
#define IS_BOOLEAN_TYPE(TYPE) ((BOOLEAN_VALUE == TYPE) || (BOOLEAN_OR_STRING_LITERAL == TYPE) || IS_NUL_VALUE(TYPE))
#define IS_STRING_TYPE(TYPE)  ((STRING_LITERAL == TYPE) || (BOOLEAN_OR_STRING_LITERAL == TYPE) || IS_NUL_VALUE(TYPE))
#define IS_DATE_TIME_TYPE(TYPE)                                                                                                   \
	((DATE_LITERAL == TYPE) || (TIME_LITERAL == TYPE) || (TIME_WITH_TIME_ZONE_LITERAL == TYPE) || (TIMESTAMP_LITERAL == TYPE) \
	 || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == TYPE))
#define IS_DATE_TIME_DATA_TYPE(TYPE)                                                                                  \
	((DATE_TYPE == TYPE) || (TIME_TYPE == TYPE) || (TIME_WITH_TIME_ZONE_TYPE == TYPE) || (TIMESTAMP_TYPE == TYPE) \
	 || (TIMESTAMP_WITH_TIME_ZONE_TYPE == TYPE))
#define IS_DATE(TYPE)	   (DATE_LITERAL == TYPE)
#define IS_TIME(TYPE)	   ((TIME_LITERAL == TYPE) || (TIME_WITH_TIME_ZONE_LITERAL == TYPE))
#define IS_TIMESTAMP(TYPE) ((TIMESTAMP_LITERAL == TYPE) || (TIMESTAMP_WITH_TIME_ZONE_LITERAL == TYPE))
#define HAS_DATE(VAL)	   (IS_TIMESTAMP(VAL) || (DATE_LITERAL == (VAL)))

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

/* The below macro is a wrapper on top of YDB_MALLOC_BUFFER but with the ability to reserve a byte at the end for the
 * null terminator. This is to be used with EXPAND_YDB_BUFFER_T_ALLOCATION whenever a null terminated string is desired.
 *
 * Note: In general, it is recommended for callers to use OCTO_MALLOC_NULL_TERMINATED_BUFFER (instead of YDB_MALLOC_BUFFER) as it
 * reserves space for a null terminator byte whereas YDB_MALLOC_BUFFER does not. And this prevents 1-byte off buffer overflows
 * (security issue) in most cases at the cost of an unnecessary byte allocation (small performance overhead) in some cases.
 * The only exception currently known in Octo is "octo_init.c" where YDB_MALLOC_BUFFER is used as it was not so straightforward
 * to do the conversion from YDB_MALLOC_BUFFER to OCTO_MALLOC_NULL_TERMINATED_BUFFER.
 */
#define OCTO_MALLOC_NULL_TERMINATED_BUFFER(BUFFER, SIZE)                              \
	{                                                                             \
		YDB_MALLOC_BUFFER(BUFFER, SIZE + 1);                                  \
		(BUFFER)->len_alloc = SIZE; /* leave room for null terminator byte */ \
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
#define AGGREGATE_DEPTH_FROM_CLAUSE	  -1
#define AGGREGATE_DEPTH_WHERE_CLAUSE	  -2
#define AGGREGATE_DEPTH_GROUP_BY_CLAUSE	  -3
#define AGGREGATE_DEPTH_HAVING_CLAUSE	  -4
#define AGGREGATE_DEPTH_UPDATE_SET_CLAUSE -5

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
		old_input_line_num = 0;                                                      \
		prev_input_line_num = 0;                                                     \
		old_input_line_begin = input_buffer_combined;                                \
		cur_input_index = 0;                                                         \
		cur_input_line_num = 0;                                                      \
		cur_input_more = &no_more;                                                   \
		leading_spaces = 0;                                                          \
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
			cur_input_line_num = 0;                                                                    \
		}                                                                                                  \
		/* if query is too long to fit in the current buffer, resize buffer                                \
		 * by min(cur_input_max * 2, QUERY_LENGTH) + padding (for the \n\0)                                \
		 */                                                                                                \
		if (QUERY_LENGTH >= (cur_input_max - cur_input_index - padding)) {                                 \
			int    resize_amt;                                                                         \
			size_t old_begin_index;                                                                    \
			char  *tmp;                                                                                \
                                                                                                                   \
			resize_amt = ((QUERY_LENGTH > (cur_input_max * 2)) ? QUERY_LENGTH : (cur_input_max * 2));  \
			tmp = malloc(resize_amt + NEWLINE_NEEDED + 1);                                             \
			memcpy(tmp, input_buffer_combined, cur_input_index);                                       \
			assert(old_input_line_begin >= input_buffer_combined);                                     \
			old_begin_index = old_input_line_begin - input_buffer_combined;                            \
			free(input_buffer_combined);                                                               \
			input_buffer_combined = tmp;                                                               \
			old_input_line_begin = &input_buffer_combined[old_begin_index];                            \
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
		cur_input_line_num = 0;                                                                             \
		old_input_index = 0;                                                                                \
		old_input_line_num = 0;                                                                             \
		/* See INIT_INPUT_BUFFER and COPY_QUERY_TO_INPUT_BUFFER macros for why the below assert is valid */ \
		assert((INIT_QUERY_SIZE - 1) <= cur_input_max);                                                     \
		memset(input_buffer_combined, 0, cur_input_max + 1);                                                \
		/* Needed to avoid incorrect syntax highlighting in error messages (YDBOcto!1157) */                \
		leading_spaces = 0;                                                                                 \
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

/* Used by `query_specification()` and `process_table_asterisk_cla()` during expansion of ASTERISK or TABLENAME.ASTERISK.
 * In case where ASTERISK or TABLENAME.ASTERISK is the first element in `column_list_alias` list (select *,n1.id ..)
 * re-initialize the head. Update `cla_cur` to point to newly inserted asterisk list.
 */
#define REPLACE_COLUMNLISTALIAS(CLA_CUR, CLA_ALIAS, CLA_HEAD, LIST)        \
	{                                                                  \
		if ((CLA_CUR) == (CLA_CUR)->next) {                        \
			(LIST)->v.column_list_alias = (CLA_ALIAS);         \
			(CLA_HEAD) = (CLA_ALIAS);                          \
		} else {                                                   \
			SqlColumnListAlias *rm_cla = (CLA_CUR);            \
                                                                           \
			(CLA_CUR) = (CLA_CUR)->next;                       \
			dqdel(rm_cla);                                     \
			dqappend(CLA_CUR, CLA_ALIAS);                      \
			if ((CLA_HEAD) == rm_cla) {                        \
				(LIST)->v.column_list_alias = (CLA_ALIAS); \
				(CLA_HEAD) = (CLA_ALIAS);                  \
			}                                                  \
		}                                                          \
		(CLA_CUR) = (CLA_ALIAS);                                   \
	}

/* Below returns the column_alias having TABLE_ASTERISK value as its column */
#define GET_TABLE_ASTERISK_COLUMN_ALIAS_FROM_COLUMN_LIST(RET, CL_STMT)            \
	{                                                                         \
		SqlColumnAlias *ca;                                               \
		SqlColumnList  *inner_column_list;                                \
		DEBUG_ONLY(SqlValue *value);                                      \
                                                                                  \
		UNPACK_SQL_STATEMENT(inner_column_list, CL_STMT, column_list);    \
		UNPACK_SQL_STATEMENT(ca, inner_column_list->value, column_alias); \
		DEBUG_ONLY(UNPACK_SQL_STATEMENT(value, ca->column, value));       \
		assert(TABLE_ASTERISK == value->type);                            \
		(RET) = ca;                                                       \
	}

/* Below parses a function_definition SQL grammar component  */
#define INVOKE_FUNCTION_DEFINITION(STMT, IDENTIFIER, FUNCTION_PARAMETER_TYPE_LIST, DATA_TYPE, M_FUNCTION, IF_NOT_EXISTS_SPECIFIED) \
	{                                                                                                                          \
		SqlStatement *ret;                                                                                                 \
                                                                                                                                   \
		ret = function_definition(IDENTIFIER, FUNCTION_PARAMETER_TYPE_LIST, DATA_TYPE, M_FUNCTION,                         \
					  IF_NOT_EXISTS_SPECIFIED);                                                                \
		if (NULL == ret) {                                                                                                 \
			YYABORT;                                                                                                   \
		}                                                                                                                  \
		STMT = ret;                                                                                                        \
	}

/* Below parses a drop_function SQL grammar component
 * FUNCTION_PARAMETER_TYPE_LIST parameter accepts $optional_function_parameter_type_list argument since its value can be NULL
 */
#define INVOKE_DROP_FUNCTION(STMT, IDENTIFIER, FUNCTION_PARAMETER_TYPE_LIST, IF_EXISTS_SPECIFIED)   \
	{                                                                                           \
		SqlStatement *ret;                                                                  \
                                                                                                    \
		ret = drop_function(IDENTIFIER, FUNCTION_PARAMETER_TYPE_LIST, IF_EXISTS_SPECIFIED); \
		if (NULL == ret) {                                                                  \
			yyerror(&yyloc, NULL, NULL, NULL, NULL, NULL);                              \
			YYABORT;                                                                    \
		}                                                                                   \
		STMT = ret;                                                                         \
	}

/* Below parses a table_reference SQL grammar component  */
#define INVOKE_TABLE_REFERENCE(STMT, COLUMN_NAME, CORRELATION_SPECIFICATION, PLAN_ID)          \
	{                                                                                      \
		SqlStatement *ret;                                                             \
		ret = table_reference(COLUMN_NAME, CORRELATION_SPECIFICATION, PLAN_ID, FALSE); \
		if (NULL == ret) {                                                             \
			YYERROR;                                                               \
		}                                                                              \
		STMT = ret;                                                                    \
	}

/* Below parses a derived_table SQL grammar component  */
#define INVOKE_DERIVED_TABLE(STMT, TABLE_SUBQUERY, CORRELATION_SPECIFICATION)   \
	{                                                                       \
		SqlStatement *ret;                                              \
		ret = derived_table(TABLE_SUBQUERY, CORRELATION_SPECIFICATION); \
		if (NULL == ret) {                                              \
			YYERROR;                                                \
		}                                                               \
		STMT = ret;                                                     \
	}

/* Below parses a drop_table_statement SQL grammar component  */
#define INVOKE_DROP_TABLE_STATEMENT(STMT, COLUMN_NAME, DROP_BEHAVIOR, DROP_DATA_RETENTION, IF_EXISTS_SPECIFIED) \
	{                                                                                                       \
		SqlStatement *ret;                                                                              \
		SQL_STATEMENT(ret, drop_table_STATEMENT);                                                       \
		OCTO_CMALLOC_STRUCT(ret->v.drop_table, SqlDropTableStatement);                                  \
		ret->v.drop_table->table_name = COLUMN_NAME;                                                    \
		ret->v.drop_table->optional_keyword = DROP_BEHAVIOR;                                            \
		ret->v.drop_table->drop_data_retention = (enum OptionalKeyword)(uintptr_t)DROP_DATA_RETENTION;  \
		ret->v.drop_table->if_exists_specified = IF_EXISTS_SPECIFIED;                                   \
		STMT = ret;                                                                                     \
	}

/* Below parses a drop_view_statement SQL grammar component  */
#define INVOKE_DROP_VIEW_STATEMENT(STMT, VIEW_NAME, IF_EXISTS_SPECIFIED)     \
	{                                                                    \
		SqlStatement *ret;                                           \
		SQL_STATEMENT(ret, drop_view_STATEMENT);                     \
		OCTO_CMALLOC_STRUCT(ret->v.drop_view, SqlDropViewStatement); \
		ret->v.drop_view->view_name = VIEW_NAME;                     \
		ret->v.drop_view->if_exists_specified = IF_EXISTS_SPECIFIED; \
		STMT = ret;                                                  \
	}

/* Below parses a drop_behavior SQL grammar component  */
#define INVOKE_DROP_BEHAVIOR(STMT, KEYWORD)        \
	{                                          \
		SqlStatement *ret;                 \
		MALLOC_KEYWORD_STMT(ret, KEYWORD); \
		ret->v.keyword->v = NULL;          \
		STMT = ret;                        \
	}

/* Below parses a truncate_table_statement SQL grammar component  */
#define INVOKE_TRUNCATE_TABLE_STATEMENT(STMT, TABLES)                                  \
	{                                                                              \
		SqlStatement *ret;                                                     \
		SQL_STATEMENT(ret, truncate_table_STATEMENT);                          \
		OCTO_CMALLOC_STRUCT(ret->v.truncate_table, SqlTruncateTableStatement); \
		ret->v.truncate_table->tables = TABLES;                                \
		STMT = ret;                                                            \
	}

/* Below parses a insert optional keyword SQL grammar component */
#define MALLOC_KEYWORD_STMT(STMT, KEYWORD)                              \
	{                                                               \
		SqlStatement *keyword;                                  \
		SQL_STATEMENT(keyword, keyword_STATEMENT);              \
		MALLOC_STATEMENT(keyword, keyword, SqlOptionalKeyword); \
		keyword->v.keyword->keyword = KEYWORD;                  \
		dqinit(keyword->v.keyword);                             \
		STMT = keyword;                                         \
	}

#ifndef NDEBUG
#define NDEBUG_ONLY(X)
#define DEBUG_ONLY(X) X
#else
#define NDEBUG_ONLY(X) X
#define DEBUG_ONLY(X)
#endif

#define MAX_TYPE_NAME_LEN 25 /* Maximum length of a type name displayed to the user. Currently its TIMESTAMP WITH TIME ZONE. */

#define INVOKE_SNPRINTF_AND_EXPAND_BUFFER_IF_NEEDED(BUFFER, BUFFER_SIZE, BUFF_PTR, ...)                            \
	{                                                                                                          \
		int index;                                                                                         \
                                                                                                                   \
		index = *BUFF_PTR - *BUFFER; /* Save current index into buffer to apply after resize, if needed */ \
		assert((0 <= index) && (*BUFFER_SIZE >= index));                                                   \
		/* Attempt to print value into buffer. If it won't fit, expand buffer and try again */             \
		*BUFF_PTR += snprintf(*BUFF_PTR, *BUFFER_SIZE - index, ##__VA_ARGS__);                             \
		while (0 >= (*BUFFER_SIZE - (*BUFF_PTR - *BUFFER))) {                                              \
			char *tmp;                                                                                 \
			int   new_size;                                                                            \
                                                                                                                   \
			new_size = *BUFFER_SIZE * 2;                                                               \
			tmp = (char *)malloc(sizeof(char) * new_size);                                             \
			memcpy(tmp, *BUFFER, *BUFFER_SIZE);                                                        \
			free(*BUFFER);                                                                             \
			*BUFFER = tmp;                                                                             \
			*BUFFER_SIZE = new_size;                                                                   \
			assert((0 <= index) && (*BUFFER_SIZE >= index));                                           \
			*BUFF_PTR = *BUFFER + index;                                                               \
			*BUFF_PTR += snprintf(*BUFF_PTR, *BUFFER_SIZE - index, ##__VA_ARGS__);                     \
		}                                                                                                  \
	}

/* Define macro to hold the maximum possible length for a user defined type (e.g. NUMERIC(25,3)) */
#define MAX_USER_VISIBLE_TYPE_STRING_LEN (MAX_TYPE_NAME_LEN + sizeof("(,)") + 2 * INT32_TO_STRING_MAX)

/* Following macro is a caller for qualify_statement() which takes care of returning immediately with 1 if the call to
 * qualify_statement() resulted in an error. `RESULT` value is used to know if the call resulted in an error.
 * This helps to issue single error per query.
 */
#define CALL_QUALIFY_STATEMENT_AND_RETURN_ON_ERROR(STMT, TABLES, TABLE_ALIAS_STMT, DEPTH, RET, RESULT) \
	{                                                                                              \
		RESULT |= qualify_statement((STMT), (TABLES), (TABLE_ALIAS_STMT), (DEPTH), (RET));     \
		if (RESULT) {                                                                          \
			return 1;                                                                      \
		}                                                                                      \
	}

/* Following macro is a caller for qualify_query() which takes care of returning immediately with 1 if the call to qualify_query()
 * resulted in an error. `RESULT` value is used to know if the call resulted in an error.
 * This helps to issue single error per query.
 */
#define CALL_QUALIFY_QUERY_AND_RETURN_ON_ERROR(TABLE_ALIAS_STMT, PARENT_JOIN, PARENT_TABLE_ALIAS, RET, RESULT) \
	{                                                                                                      \
		RESULT |= qualify_query((TABLE_ALIAS_STMT), (PARENT_JOIN), (PARENT_TABLE_ALIAS), (RET));       \
		if (RESULT) {                                                                                  \
			return 1;                                                                              \
		}                                                                                              \
	}

#define POPULATE_GVN_BUFFER_FROM_TABLE(GVN_BUFFER, TABLE, GVN_STR)                                                 \
	{                                                                                                          \
		SqlOptionalKeyword *keyword;                                                                       \
		char		   *gvname, *firstsub;                                                             \
		SqlValue	   *value;                                                                         \
                                                                                                                   \
		UNPACK_SQL_STATEMENT(keyword, (TABLE)->source, keyword);                                           \
		UNPACK_SQL_STATEMENT(value, keyword->v, value);                                                    \
		gvname = value->v.reference;                                                                       \
		firstsub = strchr(gvname, '(');                                                                    \
		if (NULL == firstsub) {                                                                            \
			/* Not sure how an unsubscripted gvn can be specified in GLOBAL. But handle it anyways. */ \
			assert(FALSE);                                                                             \
			YDB_STRING_TO_BUFFER(gvname, &(GVN_BUFFER));                                               \
		} else {                                                                                           \
			memcpy(GVN_STR, gvname, firstsub - gvname);                                                \
			GVN_STR[firstsub - gvname] = '\0';                                                         \
			YDB_STRING_TO_BUFFER(GVN_STR, &(GVN_BUFFER));                                              \
		}                                                                                                  \
	}

#define SET_OCTOLIT_BINFMT_GVN_AND_RETURN_IF_NOT_YDB_OK(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB)        \
	{                                                                                                      \
		ydb_buffer_t lcl_fmt, lcl_subs;                                                                \
		char	     lcl_fmt_buff[INT32_TO_STRING_MAX];                                                \
		YDB_STRING_TO_BUFFER(OCTOLIT_BINFMT, &lcl_subs);                                               \
		lcl_fmt.buf_addr = &lcl_fmt_buff[0];                                                           \
		lcl_fmt.len_alloc = sizeof(lcl_fmt_buff);                                                      \
		lcl_fmt.len_used = snprintf(lcl_fmt.buf_addr, lcl_fmt.len_alloc, "%d", FMT_BINARY_DEFINITION); \
		STATUS = ydb_set_s(&OCTO_GLOBAL, 1, &lcl_subs, &lcl_fmt);                                      \
		CLEANUP_AND_RETURN_IF_NOT_YDB_OK(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);              \
	}

#define UPGRADE_BINARY_DEFINITIONS_AND_RETURN_IF_NOT_YDB_OK(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB)                        \
	{                                                                                                                          \
		/* In case this is a rocto process, it is not allowed to do schema changes by default. But allow the auto upgrade  \
		 * for this process. Hence the temporary modification to "config->allow_schema_changes" below.                     \
		 */                                                                                                                \
		boolean_t save_allow_schema_changes;                                                                               \
		save_allow_schema_changes = config->allow_schema_changes;                                                          \
		config->allow_schema_changes = TRUE;                                                                               \
		assert(FALSE == config->in_auto_upgrade_binary_table_definition);                                                  \
		/* Do the actual auto upgrade of the binary function definition.                                                   \
		 * Note: Function upgrade is done prior to table upgrade to ensure that the                                        \
		 * table which depends on a function refers to the upgraded function during its upgrade.                           \
		 * This also helps to re-create all dependency nodes in the correct format.                                        \
		 */                                                                                                                \
		STATUS = auto_upgrade_binary_function_definition();                                                                \
		if (YDB_OK != STATUS) {                                                                                            \
			config->allow_schema_changes = save_allow_schema_changes;                                                  \
			CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);                                        \
		}                                                                                                                  \
		/* Do the actual auto upgrade of the binary table definition.                                                      \
		 * Set a global variable to indicate this is the small window where auto upgrade of binary table definitions       \
		 * happens. This lets "table_definition.c" know to do some special processing (use different logic to calculate    \
		 * whether a table should be considered READONLY or READWRITE).                                                    \
		 */                                                                                                                \
		config->in_auto_upgrade_binary_table_definition = TRUE;                                                            \
		STATUS = auto_upgrade_binary_table_definition();                                                                   \
		config->in_auto_upgrade_binary_table_definition = FALSE;                                                           \
		if (YDB_OK != STATUS) {                                                                                            \
			config->allow_schema_changes = save_allow_schema_changes;                                                  \
			CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);                                        \
		}                                                                                                                  \
		if (config->is_auto_upgrade_octo929) {                                                                             \
			STATUS = fclose(config->octo929_sqlfile_stream);                                                           \
			if (0 != STATUS) {                                                                                         \
				ERROR(ERR_SYSCALL_WITH_ARG, "fclose()", errno, strerror(errno), config->octo929_sqlfile);          \
				config->allow_schema_changes = save_allow_schema_changes;                                          \
				CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);                                \
			}                                                                                                          \
			STATUS = run_query_file(config->octo929_sqlfile);                                                          \
			if (0 != STATUS) {                                                                                         \
				config->allow_schema_changes = save_allow_schema_changes;                                          \
				CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);                                \
			}                                                                                                          \
			/* Now that auto upgrade of tables/functions finished fine, remove the temporary file */                   \
			STATUS = unlink(config->octo929_sqlfile);                                                                  \
			if (0 != STATUS) {                                                                                         \
				config->allow_schema_changes = save_allow_schema_changes;                                          \
				CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);                                \
			}                                                                                                          \
			/* Open a temporary file that will hold the DROP VIEW/CREATE VIEW commands needed for special auto upgrade \
			 */                                                                                                        \
			int fd;                                                                                                    \
			assert(sizeof(OCTO929_SQLFILE_NAME) < sizeof(config->octo929_sqlfile));                                    \
			strcpy(config->octo929_sqlfile, OCTO929_SQLFILE_NAME);                                                     \
			fd = mkstemp(config->octo929_sqlfile);                                                                     \
			if (-1 == fd) {                                                                                            \
				ERROR(ERR_SYSCALL_WITH_ARG, "mkstemp()", errno, strerror(errno), config->octo929_sqlfile);         \
				CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);                                \
			}                                                                                                          \
			config->octo929_sqlfile_stream = fdopen(fd, "w");                                                          \
			if (NULL == config->octo929_sqlfile_stream) {                                                              \
				ERROR(ERR_SYSCALL_WITH_ARG, "fdopen()", errno, strerror(errno), config->octo929_sqlfile);          \
				CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);                                \
			}                                                                                                          \
		}                                                                                                                  \
		config->in_auto_upgrade_binary_view_definition = TRUE;                                                             \
		STATUS = auto_upgrade_binary_view_definition();                                                                    \
		config->in_auto_upgrade_binary_view_definition = FALSE;                                                            \
		if (YDB_OK != STATUS) {                                                                                            \
			config->allow_schema_changes = save_allow_schema_changes;                                                  \
			CLEANUP_AND_RETURN(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);                                        \
		}                                                                                                                  \
		config->allow_schema_changes = save_allow_schema_changes;                                                          \
		/* Now that auto upgrade is complete, indicate that (so other processes do not attempt the auto upgrade)           \
		 * by setting ^%ydboctoocto(OCTOLIT_BINFMT) to FMT_BINARY_DEFINITION.                                              \
		 */                                                                                                                \
		SET_OCTOLIT_BINFMT_GVN_AND_RETURN_IF_NOT_YDB_OK(STATUS, RELEASE_DDL_LOCK, OCTO_GLOBAL, LOCKSUB);                   \
	}

#define DO_AUTO_UPGRADE_OCTO929_CHECK(PTR, EXPR_LEN, COLUMN, SQL_COLUMN)                              \
	{                                                                                             \
		if (config->is_auto_upgrade_octo929) {                                                \
			char  column2[OCTO_MAX_IDENT + 1];                                            \
			char *src, *srcend, *dst, *dstend;                                            \
                                                                                                      \
			src = COLUMN;                                                                 \
			srcend = COLUMN + strlen(COLUMN);                                             \
			dst = column2;                                                                \
			dstend = column2 + sizeof(column2);                                           \
			TOLOWER(dst, dstend, src, srcend);                                            \
			SQL_COLUMN = find_column(column2, table);                                     \
			if (NULL != SQL_COLUMN) {                                                     \
				/* Lower case column name matched. So modify column name in-place. */ \
				memcpy(PTR + EXPR_LEN - (srcend - src) - 2, column2, srcend - src);   \
			}                                                                             \
		}                                                                                     \
	}

#define IF_VIEW_ISSUE_UNSUPPORTED_OPERATION_ERROR(TABLE_OR_VIEW_STMT, OPERATION_STMT_TYPE) \
	{                                                                                  \
		if (create_view_STATEMENT == (TABLE_OR_VIEW_STMT)->type) {                 \
			char *op_type;                                                     \
			switch (OPERATION_STMT_TYPE) {                                     \
			case insert_STATEMENT:                                             \
				op_type = "INSERT";                                        \
				break;                                                     \
			case update_STATEMENT:                                             \
				op_type = "UPDATE";                                        \
				break;                                                     \
			case delete_from_STATEMENT:                                        \
				op_type = "DELETE";                                        \
				break;                                                     \
			default:                                                           \
				assert(FALSE);                                             \
				return NULL;                                               \
			}                                                                  \
			ERROR(ERR_VIEW_OPERATION_NOT_SUPPORTED, op_type);                  \
			return NULL;                                                       \
		}                                                                          \
	}

#define INIT_VIEW_CACHE_FOR_CURRENT_QUERY(LOADED_SCHEMAS, STATUS)                     \
	{                                                                             \
		ydb_buffer_t ydboctoView, subs_array[1];                              \
		YDB_STRING_TO_BUFFER(LOADED_SCHEMAS, &ydboctoView);                   \
		YDB_STRING_TO_BUFFER(OCTOLIT_VIEWS, &subs_array[0]);                  \
		STATUS = ydb_delete_s(&ydboctoView, 1, &subs_array[0], YDB_DEL_TREE); \
	}

#define STRTOL_VALUE_OUT_OF_RANGE(VALUE) (((LONG_MIN == VALUE) || (LONG_MAX == VALUE)) && (ERANGE == errno))

#define DATE_TIME_FORMAT_STRING(FORMAT, RET)      \
	{                                         \
		switch (FORMAT) {                 \
		case OPTIONAL_DATE_TIME_TEXT:     \
			RET = "TEXT";             \
			break;                    \
		case OPTIONAL_DATE_TIME_ZUT:      \
			RET = "ZUT";              \
			break;                    \
		case OPTIONAL_DATE_TIME_ZHOROLOG: \
			RET = "ZHOROLOG";         \
			break;                    \
		case OPTIONAL_DATE_TIME_HOROLOG:  \
			RET = "HOROLOG";          \
			break;                    \
		case OPTIONAL_DATE_TIME_FILEMAN:  \
			RET = "FILEMAN";          \
			break;                    \
		default:                          \
			assert(FALSE);            \
			RET = "";                 \
			break;                    \
		}                                 \
	}

#define GET_DATE_TIME_FORMAT_STRING_FROM_KEYWORD_TYPE(type, ret)                            \
	{                                                                                   \
		if (OPTIONAL_DATE_TIME_HOROLOG == type) {                                   \
			ret = OCTOLIT_DATE_TIME_HOROLOG;                                    \
		} else if (OPTIONAL_DATE_TIME_ZHOROLOG == type) {                           \
			ret = OCTOLIT_DATE_TIME_ZHOROLOG;                                   \
		} else if (OPTIONAL_DATE_TIME_FILEMAN == type) {                            \
			ret = OCTOLIT_DATE_TIME_FILEMAN;                                    \
		} else if (OPTIONAL_DATE_TIME_ZUT == type) {                                \
			ret = OCTOLIT_DATE_TIME_ZUT;                                        \
		} else if (OPTIONAL_DATE_TIME_TEXT == type) {                               \
			ret = OCTOLIT_DATE_TIME_TEXT;                                       \
		} else {                                                                    \
			assert(FALSE);                                                      \
			ret = ""; /* prevents [clang-diagnostic-sometimes-uninitialized] */ \
		}                                                                           \
	}

#define GET_DATE_TIME_INPUT_FORMAT_SPECIFIER_FOR_TYPE(TYPE, RET)                            \
	{                                                                                   \
		if (DATE_LITERAL == TYPE) {                                                 \
			RET = config->date_format;                                          \
		} else if (TIME_LITERAL == TYPE) {                                          \
			RET = DEFAULT_TIME_FORMAT;                                          \
		} else if (TIMESTAMP_LITERAL == TYPE) {                                     \
			RET = config->timestamp_format;                                     \
		} else if (TIME_WITH_TIME_ZONE_LITERAL == TYPE) {                           \
			RET = DEFAULT_TIMETZ_FORMAT;                                        \
		} else if (TIMESTAMP_WITH_TIME_ZONE_LITERAL == TYPE) {                      \
			RET = config->timestamptz_format;                                   \
		} else {                                                                    \
			assert(FALSE);                                                      \
			RET = ""; /* prevents [clang-diagnostic-sometimes-uninitialized] */ \
		}                                                                           \
	}
#define GET_DATE_TIME_OUTPUT_FORMAT_SPECIFIER_FOR_TYPE(TYPE, RET)                           \
	{                                                                                   \
		if (DATE_LITERAL == TYPE) {                                                 \
			RET = DEFAULT_OUTPUT_DATE_FORMAT;                                   \
		} else if (TIME_LITERAL == TYPE) {                                          \
			RET = DEFAULT_OUTPUT_TIME_FORMAT;                                   \
		} else if (TIMESTAMP_LITERAL == TYPE) {                                     \
			RET = DEFAULT_OUTPUT_TIMESTAMP_FORMAT;                              \
		} else if (TIME_WITH_TIME_ZONE_LITERAL == TYPE) {                           \
			RET = DEFAULT_OUTPUT_TIMETZ_FORMAT;                                 \
		} else if (TIMESTAMP_WITH_TIME_ZONE_LITERAL == TYPE) {                      \
			RET = DEFAULT_OUTPUT_TIMESTAMPTZ_FORMAT;                            \
		} else {                                                                    \
			assert(FALSE);                                                      \
			RET = ""; /* prevents [clang-diagnostic-sometimes-uninitialized] */ \
		}                                                                           \
	}
#define GET_EMULATION_BASED_DATE_TIME_FORMAT_SPECIFIER_FOR_TYPE(TYPE, RET)                          \
	{                                                                                           \
		if (POSTGRES == config->database_emulation) {                                       \
			if (DATE_LITERAL == TYPE) {                                                 \
				RET = POSTGRES_DATE_FORMAT;                                         \
			} else if (TIME_LITERAL == TYPE) {                                          \
				RET = POSTGRES_TIME_FORMAT;                                         \
			} else if (TIMESTAMP_LITERAL == TYPE) {                                     \
				RET = POSTGRES_TIMESTAMP_FORMAT;                                    \
			} else if (TIME_WITH_TIME_ZONE_LITERAL == TYPE) {                           \
				RET = POSTGRES_TIMETZ_FORMAT;                                       \
			} else if (TIMESTAMP_WITH_TIME_ZONE_LITERAL == TYPE) {                      \
				RET = POSTGRES_TIMESTAMPTZ_FORMAT;                                  \
			} else {                                                                    \
				assert(FALSE);                                                      \
				RET = ""; /* prevents [clang-diagnostic-sometimes-uninitialized] */ \
			}                                                                           \
		} else {                                                                            \
			assert(MYSQL == config->database_emulation);                                \
			if (DATE_LITERAL == TYPE) {                                                 \
				RET = MYSQL_DATE_FORMAT;                                            \
			} else if (TIME_LITERAL == TYPE) {                                          \
				RET = MYSQL_TIME_FORMAT;                                            \
			} else if (TIMESTAMP_LITERAL == TYPE) {                                     \
				RET = MYSQL_TIMESTAMP_FORMAT;                                       \
			} else if (TIME_WITH_TIME_ZONE_LITERAL == TYPE) {                           \
				RET = MYSQL_TIME_FORMAT;                                            \
			} else if (TIMESTAMP_WITH_TIME_ZONE_LITERAL == TYPE) {                      \
				RET = MYSQL_TIMESTAMP_FORMAT;                                       \
			} else {                                                                    \
				assert(FALSE);                                                      \
				RET = ""; /* prevents [clang-diagnostic-sometimes-uninitialized] */ \
			}                                                                           \
		}                                                                                   \
	}

#define SET_INTERNAL_FORMAT_FOR_DATA_TYPE(DATA_TYPE, INTERNAL_FORMAT) \
	{ (DATA_TYPE)->v.data_type_struct.format = (INTERNAL_FORMAT)->v.keyword->keyword; }

/* Following macro updates STMT to be a COERCE_TYPE value statement and the
 * original contents are placed as the coerce_target.
 * This macro is used by check_column_lists_for_type_match and ensure_same_type to cast
 * literals and columns to COERCED_TYPE from PRE_COERCED_TYPE.
 */
#define ADD_DATE_TIME_TIMESTAMP_CAST_STMT(STMT, COERCED_TYPE, PRE_COERCED_TYPE)                  \
	{                                                                                        \
		SqlStatement *stmt = (STMT);                                                     \
		SqlValueType  type = (COERCED_TYPE);                                             \
                                                                                                 \
		SqlStatement *ret2;                                                              \
		SQL_STATEMENT(ret2, stmt->type);                                                 \
		*ret2 = *stmt;                                                                   \
                                                                                                 \
		SqlValue *value;                                                                 \
                                                                                                 \
		MALLOC_STATEMENT(stmt, value, SqlValue);                                         \
		stmt->type = value_STATEMENT;                                                    \
		UNPACK_SQL_STATEMENT(value, stmt, value);                                        \
		value->type = COERCE_TYPE;                                                       \
		SqlStatement *dt_type;                                                           \
		SQL_STATEMENT(dt_type, data_type_struct_STATEMENT);                              \
		dt_type->v.data_type_struct.data_type = get_sqldatatype_from_sqlvaluetype(type); \
		dt_type->v.data_type_struct.size_or_precision = SIZE_OR_PRECISION_UNSPECIFIED;   \
		dt_type->v.data_type_struct.size_or_precision_parameter_index = 0;               \
		dt_type->v.data_type_struct.scale = SCALE_UNSPECIFIED;                           \
		dt_type->v.data_type_struct.scale_parameter_index = 0;                           \
		value->u.coerce_type.coerced_type = dt_type->v.data_type_struct;                 \
		value->u.coerce_type.pre_coerced_type = (PRE_COERCED_TYPE);                      \
		value->v.coerce_target = ret2;                                                   \
	}

// Convenience type definition for run_query callback function
typedef int (*callback_fnptr_t)(SqlStatement *, ydb_long_t, void *, char *, PSQL_MessageTypeT);

int emit_column_specification(char **buffer, int *buffer_size, SqlColumn *cur_column);
int emit_check_constraint(char **buffer, int *buffer_size, char **buff_ptr, struct SqlStatement *stmt);
int emit_create_table(FILE *output, struct SqlStatement *stmt);
int get_table_or_view_text_definition(ydb_buffer_t *view_or_table_name, char **text_definition);
int emit_create_function(FILE *output, struct SqlStatement *stmt);
// Recursively copies all of stmt, including making copies of strings

char *m_escape_string(const char *string);
int   m_escape_string2(char **buffer, int *buffer_len, char *string);
char *m_unescape_string(const char *string);

int	      readline_get_more(void);
SqlStatement *parse_line(ParseContext *parse_context);

int	     check_column_lists_for_type_match(SqlStatement *v,
					       ParseContext *parse_context); /* v->type is set_operation_STATEMENT or insert_STATEMENT */
int	     populate_data_type_column_list_alias(SqlStatement *v, SqlValueType *type, SqlStatement *parent_stmt, boolean_t do_loop,
						  ParseContext *parse_context, SqlColumnListAlias *fix_type);
int	     populate_data_type_cla_fix(SqlStatement *v, ParseContext *parse_context, SqlColumnListAlias *fix_type_cla);
int	     populate_data_type(SqlStatement *v, SqlValueType *type, SqlStatement *parent_stmt, ParseContext *parse_context,
				SqlValueType *fix_type);
SqlDataType  get_sqldatatype_from_sqlvaluetype(SqlValueType type);
SqlValueType get_sqlvaluetype_from_sqldatatype(SqlDataType type, boolean_t is_unknown_type_okay);
SqlValueType get_sqlvaluetype_from_psql_type(PSQL_TypeOid type);
PSQL_TypeOid get_psql_type_from_sqlvaluetype(SqlValueType type);
PSQL_TypeSize get_type_size_from_psql_type(PSQL_TypeOid type);
SqlStatement *get_deepest_column_alias_stmt(SqlStatement *new_column_alias_stmt, SqlStatement *column_alias_to_cmp_stmt);
SqlStatement *find_view_or_table(const char *table_name);
SqlFunction  *find_function(const char *function_name, const char *function_hash);
void	      get_function_name_and_parmtypes(char *ret_buff, int ret_buff_len, char *function_name, SqlStatement *parm_list_stmt);
int	   drop_schema_from_local_cache(ydb_buffer_t *name_buffer, SqlSchemaType schema_type, ydb_buffer_t *function_hash_buffer);
SqlColumn *find_column(char *column_name, SqlTable *table);
SqlStatement *find_column_alias_name(SqlStatement *stmt);
void	      parse_tree_optimize(SqlSelectStatement *select);
void	      move_where_clause_to_on_clause(SqlStatement **stmt_ptr, SqlJoin *start_join);
SqlStatement *traverse_where_clause(SqlStatement *binary_stmt, SqlJoin *start_join);

SqlColumnAlias *qualify_column_name(SqlValue *column_value, SqlJoin *tables, SqlStatement *table_alias_stmt, int depth,
				    SqlColumnListAlias **ret_cla);
int		qualify_check_constraint(SqlStatement *stmt, SqlTable *table, SqlValueType *type, SqlValueType *fix_type);
int		qualify_extract_function(SqlStatement *stmt, SqlTable *table, SqlValueType *type, boolean_t is_first_pass,
					 SqlTableAlias *table_alias, SqlStatement *column_name, SqlColumnList **dependencies);
int		qualify_query(SqlStatement *table_alias_stmt, SqlJoin *parent_join, SqlTableAlias *parent_table_alias,
			      QualifyStatementParms *ret);
int qualify_statement(SqlStatement *stmt, SqlJoin *tables, SqlStatement *table_alias_stmt, int depth, QualifyStatementParms *ret);
int qualify_view_dependency(SqlStatement *stmt, SqlView *view);

ExpressionMatchType match_expression(char *start, char *column, int *expr_len, int max_column_len, char prev);

SqlColumnListAlias *match_column_in_table(SqlTableAlias *table, char *column_name, int column_name_len, boolean_t *ambiguous,
					  boolean_t issue_error);
boolean_t	    match_column_list_alias_in_select_column_list(SqlColumnListAlias *match_cla, SqlStatement *cla_stmt);
SqlOptionalKeyword *get_keyword(SqlColumn *column, enum OptionalKeyword keyword);
char		   *get_keyword_name(enum OptionalKeyword keyword);
SqlOptionalKeyword *get_keyword_from_keywords(SqlOptionalKeyword *start_keyword, enum OptionalKeyword keyword);
int		    get_key_columns(SqlTable *table, SqlColumn **key_columns);
int  generate_key_name(char **buffer, int *buffer_size, int target_key_num, SqlTable *table, SqlColumn **key_columns);
int  get_row_count_from_cursorId(ydb_long_t cursorId);
int  print_temporary_table(SqlStatement *, ydb_long_t cursorId, void *parms, char *plan_name, PSQL_MessageTypeT msg_type);
void print_result_row(ydb_buffer_t *row);
int  get_mval_len(unsigned char *buff, int *data_len);
int  truncate_table_tp_callback_fn(SqlStatement *truncate_stmt);

/**
 * Parses query, and calls the callback if it is a select statement. Otherwise, the query is a data altering
 *  query and gets executed
 *
 * @returns TRUE on success, FALSE on failure
 */
int run_query(callback_fnptr_t callback, void *parms, PSQL_MessageTypeT msg_type, ParseContext *parse_context);

/* Executes a sequence of SQL queries stored in file name "query_file_name" */
int run_query_file(char *query_file_name);

char	     *get_aggregate_func_name(SqlAggregateType type);
SqlValueType  get_set_operation_column_alias_type(SqlStatement *ca_stmt);
char	     *get_set_operation_string(SqlSetOperationType type);
char	     *get_user_visible_binary_operator_string(enum BinaryOperations operation);
int	      get_user_visible_data_type_string(SqlDataTypeStruct *data_type_ptr, char *ret_buff, int ret_buff_size);
char	     *get_user_visible_type_string(SqlValueType type);
char	     *get_user_visible_unary_operator_string(enum UnaryOperations operation);
SqlStatement *get_display_relation_query_stmt(SqlDisplayRelationType relation_type, ParseContext *parse_context);

// Implements the "\d tablename/viewname" command at the OCTO> prompt
int describe_table_or_view_name(SqlStatement *table_name);

/* Displays the GLOBAL that holds the table's records. */
void describe_tablename_global(SqlTable *table);

// GROUP BY expression support functions
int		   get_group_by_column_number(SqlTableAlias *table_alias, SqlStatement *hash_to_match);
group_by_fields_t *get_group_by_fields(SqlStatement *stmt);

/* Hashing support functions */
void generate_name_type(NameType file_type, hash128_state_t *state, int len, char *routine_name, int routine_len);
void hash_canonical_query(hash128_state_t *state, SqlStatement *stmt, int *status);
void ydb_hash_to_string(ydb_uint16 *hash, char *buffer, const unsigned int buf_len);

SqlOptionalKeyword *add_optional_piece_keyword_to_sql_column(int column_number);

SqlColumnListAlias *get_encapsulated_cla_list(SqlStatement *table_stmt, SqlStatement *table_alias_stmt);

SqlStatement	   *drill_to_table_alias(SqlStatement *sqlStmt);
SqlColumn	   *get_column_under_column_list_alias(SqlColumnListAlias *cla);
int		    get_column_piece_number(SqlColumnAlias *column_alias, SqlTableAlias *table_alias);
int		    get_column_number_from_column_list_alias(SqlColumnListAlias *input_cla, SqlTableAlias *table_alias);
SqlColumnListAlias *get_column_list_alias_n_from_table_alias(SqlTableAlias *table_alias, int column_number);
SqlColumnAlias	   *get_column_alias_for_column_list_alias(SqlColumnListAlias *col_cla, SqlStatement *matching_alias_stmt);
int		    get_num_cols_in_table_alias(SqlTableAlias *table_alias);
SqlColumnAlias	   *get_column_alias_from_column_name(char *columnName, SqlTableAlias *table_alias);

SqlColumnListAlias *copy_column_list_alias_list(SqlColumnListAlias *cla, SqlStatement *sql_stmt, SqlStatement *keywords);
SqlStatement	   *copy_sql_statement(SqlStatement *stmt);
boolean_t	    match_sql_statement(SqlStatement *stmt, SqlStatement *match_stmt);

void	      compress_statement(SqlStatement *stmt, char **out, int *out_length, boolean_t is_view_processing);
SqlStatement *decompress_statement(char *buffer, int out_length);

int   store_table_definition(ydb_buffer_t *table_name_buff, char *table_defn, int table_defn_length, boolean_t is_text);
int   store_function_definition(ydb_buffer_t *function_name_buffers, char *function_defn, int function_defn_length,
				boolean_t is_text);
int   store_function_dependencies(char *table_name, DDLDependencyType dtype);
int   store_table_or_view_in_pg_class(SqlStatement *table_or_view_stmt, ydb_buffer_t *name_buffer);
int   store_view_dependencies(char *view_name, ydb_buffer_t *view_name_buffer);
int   store_table_dependencies(SqlTable *table, char *table_name, ydb_buffer_t *table_name_buffer);
int   delete_table_or_view_from_pg_class(ydb_buffer_t *table_name_buffer);
void  cleanup_tables(void);
int   store_function_in_pg_proc(SqlFunction *function, char *function_hash);
int   delete_function_from_pg_proc(ydb_buffer_t *function_name_buffer, ydb_buffer_t *function_hash_buffer);
int   regex_has_no_special_characters(SqlStatement *op1, enum RegexType regex_type, ParseContext *parse_context);
int   store_plandirs_gvn(char *plan_filename);
char *get_date_time_format_string(enum OptionalKeyword keyword);

/* Parse related functions invoked from the .y files (parser.y, select.y etc.) */
SqlStatement *sql_set_statement(SqlStatement *variable, SqlStatement *value, ParseContext *parse_context);
SqlStatement *aggregate_function(SqlAggregateType aggregate_type, OptionalKeyword set_quantifier, SqlStatement *value_expression,
				 YYLTYPE *loc);
SqlStatement *alloc_keyword_of_type(OptionalKeyword keyword_type);
SqlStatement *between_predicate(SqlStatement *row_value_constructor, SqlStatement *from, SqlStatement *to, boolean_t not_specified);
SqlStatement *cast_specification(SqlStatement *cast_specification, SqlStatement *source);
SqlStatement *create_sql_column_list(SqlStatement *elem, SqlStatement *tail, YYLTYPE *llocp);
int	      copy_correlation_specification_aliases(SqlTableAlias *table_alias);
SqlStatement *data_type(SqlDataType data_type, SqlStatement *size_or_precision, SqlStatement *scale);
SqlStatement *derived_column(SqlStatement *derived_column_expression, SqlStatement *column_name, struct YYLTYPE *yloc);
SqlStatement *derived_table(SqlStatement *table_subquery, SqlStatement *correlation_specification);
SqlStatement *grouping_column_reference(SqlStatement *derived_column_expression, SqlStatement *collate_clause);
SqlStatement *insert_statement(SqlStatement *table_name, SqlStatement *column_name_list, SqlStatement *optional_words,
			       SqlStatement *query_expression, int *plan_id, ParseContext *parse_context);
SqlStatement *delete_from_statement(SqlStatement *table_name, SqlStatement *alias_name, SqlStatement *where_clause, int *plan_id,
				    ParseContext *parse_context);
SqlStatement *update_statement(SqlStatement *table_name, SqlStatement *alias_name, SqlStatement *set_clause_list,
			       SqlStatement *where_clause, int *plan_id, ParseContext *parse_context);
int	      natural_join_condition(SqlJoin *start, SqlJoin *r_join);
int	      parse_literal_to_parameter(ParseContext *parse_context, SqlValue *value, boolean_t update_existing);
SqlStatement *query_specification(OptionalKeyword set_quantifier, SqlStatement *select_list, SqlStatement *table_expression,
				  SqlStatement *sort_specification_list, int *plan_id);
SqlStatement *validate_query_expression(SqlStatement *query_expression, ParseContext *parse_context, SqlStatementType cmd_type);
SqlColumnListAlias *process_asterisk(SqlJoin *select_table_list, struct YYLTYPE loc);
void	  process_table_asterisk_cla(SqlStatement *specification_list, SqlColumnListAlias **cla_cur, SqlColumnListAlias **cla_head,
				     QualifyQueryStage qualify_query_stage);
void	  process_aggregate_function_table_asterisk(SqlAggregateFunction *af);
boolean_t is_stmt_table_asterisk(SqlStatement *stmt);
int regex_specification(SqlStatement **stmt, SqlStatement *op0, SqlStatement *op1, enum RegexType regex_type, int is_sensitive,
			int is_not);
SqlStatement *set_operation(enum SqlSetOperationType setoper_type, SqlStatement *left_operand, SqlStatement *right_operand);
SqlStatement *sort_specification(SqlStatement *sort_key, SqlStatement *ordering_specification);
SqlStatement *table_definition(SqlStatement *tableName, SqlStatement *table_element_list, SqlStatement *table_definition_tail,
			       boolean_t is_not_exists_specified);
SqlStatement *view_definition(SqlStatement *create_view_stmt, ParseContext *parse_context);
SqlStatement *table_expression(SqlStatement *from, SqlStatement *where, SqlStatement *group_by, SqlStatement *having);
SqlStatement *table_reference(SqlStatement *column_name, SqlStatement *correlation_specification, int *plan_id,
			      boolean_t only_table_possible);
SqlStatement *function_definition(SqlStatement *identifier, SqlStatement *function_parameter_type_list, SqlStatement *data_type,
				  SqlStatement *m_function, boolean_t if_not_exists_specified);
SqlStatement *drop_function(SqlStatement *identifier, SqlStatement *function_parameter_type_list, boolean_t if_exists_specified);

void constraint_name_auto_generate(SqlOptionalKeyword *cur_keyword, char *table_name, char *column_name, int numeric_suffix,
				   char *name_buf, int buf_size);
int  compare_column_count_and_column_type_of_tables(SqlColumnAlias *first_column_alias, SqlColumnAlias *second_column_alias,
						    ParseContext *parse_context);
int  validate_table_asterisk_binary_operation(SqlBinaryOperation *binary, SqlValueType orig_child_type[2],
					      ParseContext *parse_context);
int  validate_global_keyword(SqlOptionalKeyword *keyword, SqlTable *table, int max_key);
int  validate_start_end_keyword(SqlOptionalKeyword *keyword, SqlTable *table);
int  validate_date_time_value(char **literal_ptr, SqlValueType date_time_type, OptionalKeyword internal_format, char *text_format);
int  set_date_time_format_from_datestyle(char *date_style_value);

boolean_t table_has_hidden_column(SqlTable *table);

// Updates a runtime parameter value in `pg_catalog.pg_settings`. Executed for SQL SET commands.
int set_parameter_in_pg_settings(char *variable, char *value);
// Displays a runtime parameter value in `pg_catalog.pg_settings`. Executed for SQL SHOW commands.
char *get_parameter_from_pg_settings(char **variable, ydb_buffer_t *out);

// Creates a new cursor by assigning a new cursorId
int64_t	  create_cursor(ydb_buffer_t *schema_global, ydb_buffer_t *cursor_buffer);
boolean_t is_query_canceled(callback_fnptr_t callback);

/* Loads default runtime parameter settings into `pg_catalog.pg_settings`.
 * Needed for SET/SHOW commands to work properly.
 */
int load_pg_defaults(void);

// Returns the amount of memory used by the current process
int64_t get_mem_usage(void);

int no_more(void);

int  get_input(char *buf, int size);
void yyerror(YYLTYPE *llocp, yyscan_t scan, SqlStatement **out, int *plan_id, ParseContext *parse_context, char const *s);

int   get_full_path_of_generated_m_file(char *filename, int filename_len, char *m_routine_name);
int   get_full_path_of_generated_o_file(char *filename, int filename_len, char *o_routine_name);
char *get_emulation_string(void);

int auto_load_octo_seed(void);
int auto_load_octo_seed_if_needed(void);
int auto_upgrade_plan_definition_if_needed(void);
int auto_upgrade_binary_definition_if_needed(void);
int auto_upgrade_binary_function_definition(void);
int auto_upgrade_binary_table_definition(void);
int auto_upgrade_binary_view_definition(void);
int auto_upgrade_binary_table_or_view_definition_helper(ydb_buffer_t *view_or_table_name, boolean_t is_view);
int is_auto_upgrade_valid(void);
int ensure_seed_objects_are_not_dropped(SqlStatement *result);

/* history.c function prototypes */
void print_history(void);
void load_readline_history(void);
void save_readline_history(void);
void set_readline_file(void);
void set_octo_history_max_length(void);
void readline_setup(void);
void add_single_history_item(char *input_buffer_combined, int old_input_index);

/* Globals */
extern uint64_t hash_canonical_query_cycle;	// incremented before every outermost call to "hash_canonical_query"
extern uint64_t qualify_extract_function_cycle; // incremented before every outermost call to "qualify_extract_function"
extern int	cur_input_index;		// Current index of input_buffer_combined the parser should read from,
						// and readlines should write to. Effectively marks the end of the
						// current query.
extern int old_input_index;			// The previous value of cur_input_index before the parser modifies it.
						// Effectively marks the start of the current query.
extern int   old_input_line_num;		// The line number pointed to by old_input_index
extern int   prev_input_line_num;		// The line number pointed to by the previous value of old_input_index
extern char *old_input_line_begin;		// Pointer to the beginning of the line pointed to by old_input_index
extern int   leading_spaces;			// leading spaces in the current query it needs to be stored somewhere
						// accessible but should be ignored, except by the lexer and yyerror
extern int   cur_input_line_num;		// The line number pointed to by cur_input_index
extern int   cur_input_max;
extern int   eof_hit;
extern FILE *inputFile;
extern char *input_buffer_combined; // The input buffer for octo. Contains the query strings.
extern int (*cur_input_more)(void);
extern OctoConfig  *config;
extern ydb_buffer_t lex_buffer;		// String buffer for use in lexer.l
extern int	    ydb_release_number; /* e.g. the integer 130 in case of r1.30 etc. */
extern boolean_t    in_sql_transaction; // TRUE if inside a BEGIN/COMMIT transaction fence. FALSE otherwise.

#endif
