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

#ifndef OCTO_TYPES_H
#define OCTO_TYPES_H

#include <stdint.h> /* needed for uint64_t */

typedef void *yyscan_t;

#include <libyottadb.h>

#include "memory_chunk.h"
#include "double_list.h"

// Set maximum M routine length - must be in sync with MAX_MIDENT_LEN in YDB/sr_port/mdef.h
#define MAX_ROUTINE_LEN YDB_MAX_IDENT

/* Note: The below macro needs to be kept in sync with
 * 1) MAX_SRCLINE in YDB/sr_port/compiler.h AND
 * 2) YDB_MAX_M_LINE_LEN in YDB/sr_unix/libyottadb.h
 *
 * It uses a hardcoded integer value below, not the YDB_MAX_M_LINE_LEN macro (which is available in libyottadb.h)
 * because that macro would be available only in libyottadb.h of YottaDB r2.00 onwards and we want
 * Octo builds to continue to work with r1.38 and prior versions.
 *
 * This macro is currently not used but it is hoped that it will be useful when examining emitted M code and wanting
 * to decide whether it is longer than the maximum allowed M line limit in YottaDB. In that case, the M code emitting
 * logic can backtrack and decide on an alternative approach (for example, splitting the emitted M line into 2 or so).
 */
#define MAX_M_LINE_LEN 32766

// Per https://www.postgresql.org/docs/11/catalog-pg-type.html, for type length values of -1 and -2:
//	"-1 indicates a 'varlena' type (one that has a length word), -2 indicates a null-terminated C string."
#define TYPLEN_VARLENA -1
#define TYPLEN_CSTRING -2

// Allocates ONE structure of type TYPE
#define OCTO_CMALLOC_STRUCT(RET, TYPE) \
	{ RET = (TYPE *)octo_cmalloc(memory_chunks, sizeof(TYPE)); }

#define SQL_STATEMENT(VAR, TYPE)                        \
	{                                               \
		OCTO_CMALLOC_STRUCT(VAR, SqlStatement); \
		(VAR)->type = TYPE;                     \
	}

#define MALLOC_STATEMENT(VAR, NAME, TYPE) \
	{ OCTO_CMALLOC_STRUCT((VAR)->v.NAME, TYPE); }

#define UNPACK_SQL_STATEMENT(result, item, StatementType)          \
	{                                                          \
		assert(NULL != item);                              \
		assert((item)->type == StatementType##_STATEMENT); \
		(result) = (item)->v.StatementType;                \
	}

#define PACK_SQL_STATEMENT(out, item, StatementType)           \
	{                                                      \
		SQL_STATEMENT(out, StatementType##_STATEMENT); \
		(out)->v.StatementType = item;                 \
	}

#define SHALLOW_COPY_SQL_STATEMENT(dst, src, NAME, TYPE) \
	do {                                             \
		SQL_STATEMENT((dst), src->type);         \
		MALLOC_STATEMENT((dst), NAME, TYPE);     \
		*(dst)->v.NAME = *(src)->v.NAME;         \
	} while (0);

/* Determines the corresponding (SqlStatement *) structures that points to a (SqlTable/SqlView *) structure */
#define SQL_STATEMENT_FROM_SQLTABLE_OR_SQLVIEW(ALIAS, TABLE_OR_VIEW)                      \
	{                                                                                 \
		SqlStatement *lcl_ret;                                                    \
		lcl_ret = (SqlStatement *)((char *)TABLE_OR_VIEW - sizeof(SqlStatement)); \
		if (create_view_STATEMENT == lcl_ret->type) {                             \
			/* Cast to avoid compiler warnings */                             \
			lcl_ret->v.create_view = (SqlView *)TABLE_OR_VIEW;                \
		} else if (create_table_STATEMENT == lcl_ret->type) {                     \
			/* Cast to avoid compiler warnings */                             \
			lcl_ret->v.create_table = (SqlTable *)TABLE_OR_VIEW;              \
		} else {                                                                  \
			assert(FALSE);                                                    \
		}                                                                         \
		(ALIAS)->table = lcl_ret;                                                 \
	}

#define SQL_VALUE_STATEMENT(DEST, TYPE, STRING_LITERAL)          \
	{                                                        \
		SqlStatement *lcl_ret;                           \
		SqlValue *    lcl_value;                         \
                                                                 \
		SQL_STATEMENT(lcl_ret, value_STATEMENT);         \
		MALLOC_STATEMENT(lcl_ret, value, SqlValue);      \
		UNPACK_SQL_STATEMENT(lcl_value, lcl_ret, value); \
		lcl_value->type = TYPE;                          \
		lcl_value->v.string_literal = STRING_LITERAL;    \
		assert(FALSE == lcl_value->is_double_quoted);    \
		DEST = lcl_ret;                                  \
	}

#define SQL_VALUE_MALLOC_STATEMENT(DEST, TYPE, COPIED)                            \
	{                                                                         \
		char *string_literal;                                             \
                                                                                  \
		string_literal = (char *)octo_cmalloc(memory_chunks, COPIED + 1); \
		SQL_VALUE_STATEMENT(DEST, TYPE, string_literal);                  \
	}

#define SQL_COLUMN_LIST_ALIAS_STATEMENT(DEST)                                     \
	{                                                                         \
		SqlStatement *lcl_ret;                                            \
                                                                                  \
		SQL_STATEMENT(lcl_ret, column_list_alias_STATEMENT);              \
		MALLOC_STATEMENT(lcl_ret, column_list_alias, SqlColumnListAlias); \
		dqinit(lcl_ret->v.column_list_alias);                             \
		DEST = lcl_ret;                                                   \
	}

#define SQL_COLUMN_LIST_STATEMENT(DEST)                                \
	{                                                              \
		SqlStatement *lcl_ret;                                 \
                                                                       \
		SQL_STATEMENT(lcl_ret, column_list_STATEMENT);         \
		MALLOC_STATEMENT(lcl_ret, column_list, SqlColumnList); \
		dqinit(lcl_ret->v.column_list);                        \
		DEST = lcl_ret;                                        \
	}

/* Shamelessly stolen from mlkdef.h in YottaDB */
/* convert relative pointer to absolute pointer */
#define R2A(X) (void *)(((unsigned char *)&(X)) + ((size_t)X))

/* store absolute pointer Y in X as a relative pointer */
#define A2R(X) ((X) = (void *)(((unsigned char *)(X)) - ((unsigned char *)&(X))))

#define IS_OUTER_JOIN(JOIN_TYPE) ((LEFT_JOIN == JOIN_TYPE) || (RIGHT_JOIN == JOIN_TYPE) || (FULL_JOIN == JOIN_TYPE))

typedef long long unsigned int uint8;

typedef int boolean_t;

typedef enum NameType {
	CrossReference, /* The name of the cross reference plan M file (_ydboctoX....m) */
	OutputPlan,	/* The name of the output plan M file (_ydboctoP...m) */
	FunctionHash,	/* The name for a CREATE FUNCTION definition (_ydboctoF...) */
	TableGlobal,	/* The global name that stores CREATE TABLE data if GLOBAL keyword is not specified (_ydboctoD...) */
	UniqueGlobal,	/* The global name that stores UNIQUE constraint enforcement (_ydboctoU...) */
} NameType;

/* Note: The order of the statement types listed below is the same as the order of fields listed under
 * the union inside "typedef struct SqlStatement" in a different section of this same file ("octo_types.h").
 */
typedef enum SqlStatementType {
	create_table_STATEMENT,
	create_function_STATEMENT,
	select_STATEMENT,
	insert_STATEMENT,
	drop_table_STATEMENT,
	drop_function_STATEMENT,
	value_STATEMENT,
	function_call_STATEMENT,
	coalesce_STATEMENT,
	greatest_STATEMENT,
	least_STATEMENT,
	null_if_STATEMENT,
	aggregate_function_STATEMENT,
	binary_STATEMENT,
	unary_STATEMENT,
	column_list_STATEMENT,
	column_STATEMENT,
	join_STATEMENT,
	parameter_type_list_STATEMENT,
	constraint_STATEMENT,
	keyword_STATEMENT,
	column_list_alias_STATEMENT,
	column_alias_STATEMENT,
	table_alias_STATEMENT,
	set_operation_STATEMENT,
	begin_STATEMENT,
	commit_STATEMENT,
	cas_STATEMENT,
	cas_branch_STATEMENT,
	set_STATEMENT,
	show_STATEMENT,
	no_data_STATEMENT,
	delim_char_list_STATEMENT,
	index_STATEMENT,
	data_type_struct_STATEMENT,
	join_type_STATEMENT,
	discard_all_STATEMENT,
	row_value_STATEMENT,
	table_value_STATEMENT,
	array_STATEMENT,
	history_STATEMENT,
	delete_from_STATEMENT,
	update_STATEMENT,
	display_relation_STATEMENT,
	truncate_table_STATEMENT,
	create_view_STATEMENT,
	drop_view_STATEMENT,
	dynamic_sql_STATEMENT,
	invalid_STATEMENT, // Keep invalid_STATEMENT at the end
} SqlStatementType;

// The order of these must be kept in sync with `LPActionType` in `src/optimization_transforms/lp_action_type.hd`
typedef enum UnaryOperations {
	FORCE_NUM,
	NEGATIVE,
	BOOLEAN_NOT,
	BOOLEAN_EXISTS,
	BOOLEAN_NOT_EXISTS, // Not used but needed to be in sync with LP_BOOLEAN_NOT_EXISTS in `lp_action_type.hd`
} UnaryOperations;

// The order of these must be kept in sync with `LPActionType` in `src/optimization_transforms/lp_action_type.hd`
typedef enum BinaryOperations {
	ADDITION,
	SUBTRACTION,
	DIVISION,
	MULTIPLICATION,
	MODULO,
	CONCAT,
	BOOLEAN_OR,
	BOOLEAN_AND,
	BOOLEAN_IS,
	BOOLEAN_IS_NOT,
	BOOLEAN_EQUALS,
	BOOLEAN_NOT_EQUALS,
	BOOLEAN_LESS_THAN,
	BOOLEAN_GREATER_THAN,
	BOOLEAN_LESS_THAN_OR_EQUALS,
	BOOLEAN_GREATER_THAN_OR_EQUALS,
	BOOLEAN_REGEX_SENSITIVE,
	BOOLEAN_REGEX_INSENSITIVE,
	BOOLEAN_REGEX_SENSITIVE_LIKE,
	BOOLEAN_REGEX_INSENSITIVE_LIKE,
	BOOLEAN_REGEX_SENSITIVE_SIMILARTO,
	BOOLEAN_REGEX_INSENSITIVE_SIMILARTO,
	BOOLEAN_IN,
	BOOLEAN_NOT_IN,
	BOOLEAN_ANY_EQUALS,
	BOOLEAN_ANY_NOT_EQUALS,
	BOOLEAN_ANY_LESS_THAN,
	BOOLEAN_ANY_GREATER_THAN,
	BOOLEAN_ANY_LESS_THAN_OR_EQUALS,
	BOOLEAN_ANY_GREATER_THAN_OR_EQUALS,
	BOOLEAN_ALL_EQUALS,
	BOOLEAN_ALL_NOT_EQUALS,
	BOOLEAN_ALL_LESS_THAN,
	BOOLEAN_ALL_GREATER_THAN,
	BOOLEAN_ALL_LESS_THAN_OR_EQUALS,
	BOOLEAN_ALL_GREATER_THAN_OR_EQUALS,
} BinaryOperations;

typedef enum SqlValueType {
	UNKNOWN_SqlValueType,
	BOOLEAN_VALUE,
	NUMERIC_LITERAL,
	INTEGER_LITERAL,
	STRING_LITERAL,
	COLUMN_REFERENCE,
	CALCULATED_VALUE,
	FUNCTION_NAME,
	FUNCTION_HASH,
	PARAMETER_VALUE,
	NUL_VALUE, /* NULL SQL keyword or '' (empty string) */
	COERCE_TYPE,
	DELIM_VALUE,
	IS_NULL_LITERAL, /* Special type to correspond to value of NULL as part of an IS NULL.
			  * This is treated as an empty string `""` or `$ZYSQLNULL` based on
			  * the context. This duality is why it is different from a NUL_VALUE type.
			  */
	TABLE_ASTERISK,
	BOOLEAN_OR_STRING_LITERAL, /* Strings like 't'/'f' are BOOLEAN or STRINGs depending on the context.
				    * They start with this state and are later reset to BOOLEAN_VALUE if the
				    * context makes them a boolean value and if not they are reset to STRING_LITERAL.
				    * So this is a temporary state that is only there for a short time during parsing.
				    */
	SELECT_ASTERISK,	   /* To note down a "*" specified in a SELECT column list. This exists only for a short time
				    * until it is expanded to the list of columns derived from the FROM/JOIN list of tables.
				    * So most of the code after early parsing should expect to not see this at all.
				    */
	INVALID_SqlValueType
} SqlValueType;

/* Store the fact that SIZE, PRECISION, SCALE for column types were not specified in the CREATE TABLE
 * using a negative value (i.e. very large positive value).
 */
#define SIZE_OR_PRECISION_UNSPECIFIED -1
#define SCALE_UNSPECIFIED	      SIZE_OR_PRECISION_UNSPECIFIED

typedef enum SqlDataType { UNKNOWN_SqlDataType, BOOLEAN_TYPE, INTEGER_TYPE, NUMERIC_TYPE, STRING_TYPE, NUL_TYPE } SqlDataType;

/* Note: Additions of keywords in the middle of the table can cause SIG-11s because the actual binary value
 *       of these enums (e.g. PRIMARY_KEY) is stored in the ^%ydboctoschema(<tablename>,OCTOLIT_BINARY,*) global nodes
 *       and using a newer build of Octo without killing ^%ydboctoschema could load a table definition that
 *       is out of date with respect to the newer build.
 * Note: Any additions/deletions to this list might need to be correspondingly changed in "lp_emit_plan.c".
 */
typedef enum OptionalKeyword {
	NO_KEYWORD,
	PRIMARY_KEY,
	NOT_NULL,
	UNIQUE_CONSTRAINT,
	OPTIONAL_GLOBAL,
	OPTIONAL_END,
	OPTIONAL_START,
	OPTIONAL_DELIM,
	OPTIONAL_EXTRACT,
	OPTIONAL_CASCADE,
	OPTIONAL_RESTRICT,
	OPTIONAL_PIECE,
	OPTIONAL_KEY_NUM,
	OPTIONAL_ADVANCE, /* Corresponds to nixed ADVANCE keyword. Not deleted for backward compatibility just in case. */
	OPTIONAL_LIMIT,
	OPTIONAL_DISTINCT,
	OPTIONAL_XREF_INDEX,	    // not sure if this should be here; gets populated through LP
	OPTIONAL_BOOLEAN_EXPANSION, // indicates that this statement is part of an OR boolean expression expansion to BNF form
	OPTIONAL_ASC,
	OPTIONAL_DESC,
	OPTIONAL_STARTINCLUDE,
	OPTIONAL_READONLY,
	OPTIONAL_READWRITE,
	OPTIONAL_ENDPOINT,
	OPTIONAL_KEEPDATA,
	OPTIONAL_CHECK_CONSTRAINT,
	OPTIONAL_AIM_TYPE,
	OPTIONAL_OVERRIDING_SYSTEM_VALUE,
	OPTIONAL_OVERRIDING_USER_VALUE,
	OPTIONAL_DEFAULT,
	OPTIONAL_GENERATED_BY_DEFAULT_IDENTITY,
	OPTIONAL_GENERATED_ALWAYS_IDENTITY,
	OPTIONAL_MAYBE_CANONICAL,
} OptionalKeyword;

typedef enum SqlSetOperationType {
	SET_UNION,
	SET_UNION_ALL,
	SET_EXCEPT,
	SET_EXCEPT_ALL,
	SET_INTERSECT,
	SET_INTERSECT_ALL
} SqlSetOperationType;

typedef enum SqlJoinType { NO_JOIN, NATURAL_JOIN, CROSS_JOIN, INNER_JOIN, RIGHT_JOIN, LEFT_JOIN, FULL_JOIN } SqlJoinType;

/* Note: Order of the below enums should be kept in sync with order of `LP_AGGREGATE_FUNCTION_*` types in `lp_action_type.hd` */
typedef enum SqlAggregateType {
	AGGREGATE_COUNT_ASTERISK,
	AGGREGATE_COUNT,
	AGGREGATE_SUM,
	AGGREGATE_AVG,
	AGGREGATE_MIN,
	AGGREGATE_MAX,
	AGGREGATE_COUNT_DISTINCT,
	AGGREGATE_SUM_DISTINCT,
	AGGREGATE_AVG_DISTINCT,
	AGGREGATE_COUNT_DISTINCT_TABLE_ASTERISK,
	AGGREGATE_COUNT_TABLE_ASTERISK,
	/* Note: AGGREGATE_MIN_DISTINCT is equivalent to AGGREGATE_MIN */
	/* Note: AGGREGATE_MAX_DISTINCT is equivalent to AGGREGATE_MAX */
	AGGREGATE_LAST
} SqlAggregateType;

typedef enum SqlDisplayRelationType {
	DISPLAY_ALL_RELATION,
	DISPLAY_ALL_VIEW_RELATION,
	DISPLAY_TABLE_RELATION
} SqlDisplayRelationType;

/* Values for this enum are derived from the PostgreSQL catalog and only include types Octo currently supports.
 * Any additions to the below list might need changes to "pgFormatType" entryref in "src/aux/_ydboctopgfunctions.m".
 *
 * Typename to OID mappings can be acquired by running the following
 * query against an existing PostgreSQL database:
 *	select typname,oid from pg_catalog.pg_type;
 */
typedef enum {
	PSQL_TypeOid_bool = 16,
	PSQL_TypeOid_int8 = 20,
	PSQL_TypeOid_int2 = 21,
	PSQL_TypeOid_int4 = 23,
	PSQL_TypeOid_unknown = 705,
	PSQL_TypeOid_varchar = 1043,
	PSQL_TypeOid_numeric = 1700,
} PSQL_TypeOid;

// Values for this enum are derived from the PostgreSQL catalog and
// only include types Octo currently supports.
// Typename to type size mappings can be acquired by running the following
// query against an existing PostgreSQL database:
//	select oid,typname,typlen from pg_catalog.pg_type;
typedef enum {
	PSQL_TypeSize_unknown = TYPLEN_CSTRING,
	PSQL_TypeSize_numeric = TYPLEN_VARLENA,
	PSQL_TypeSize_varchar = TYPLEN_VARLENA,
	PSQL_TypeSize_bool = 1,
	PSQL_TypeSize_int4 = 4,
} PSQL_TypeSize;

/* Source: https://www.postgresql.org/docs/current/protocol-message-formats.html
 * The PSQL_Invalid type below is an Octo-specific addition that is not part of the protocol.
 */
typedef enum PSQL_MessageTypes {
	PSQL_Invalid = 0,
	PSQL_Authenication = 'R',
	PSQL_AuthenticationMD5Password = 'R',
	PSQL_AuthenticationOk = 'R',
	PSQL_BackendKeyData = 'K',
	PSQL_Bind = 'B',
	PSQL_BindComplete = '2',
	PSQL_Close = 'C',
	PSQL_CloseComplete = '3',
	PSQL_CommandComplete = 'C',
	PSQL_DataRow = 'D',
	PSQL_Describe = 'D',
	PSQL_EmptyQueryResponse = 'I',
	PSQL_ErrorResponse = 'E',
	PSQL_Execute = 'E',
	PSQL_Flush = 'H',
	PSQL_NoData = 'n',
	PSQL_NoticeResponse = 'N',
	PSQL_Query = 'Q',
	PSQL_ReadyForQuery = 'Z',
	PSQL_RowDescription = 'T',
	PSQL_ParameterStatus = 'S',
	PSQL_Parse = 'P',
	PSQL_ParseComplete = '1',
	PSQL_PasswordMessage = 'p',
	PSQL_PortalSuspended = 's',
	PSQL_Sync = 'S',
	PSQL_ParameterDescription = 't',
	PSQL_Terminate = 'X'
} PSQL_MessageTypeT;

// Simple enum for distinguishing between different types of schemas when caching
typedef enum {
	TableSchema,
	FunctionSchema,
} SqlSchemaType;

#define YYLTYPE yyltype

typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE {
	int first_line;
	int first_column;
	int last_line;
	int last_column;
};

// Used to maintain various parse related information primarily for use in Extended Query protocol modules
typedef struct {
	// General purpose parser fields
	ydb_long_t cursorId;
	char *	   cursorIdString;
	boolean_t  abort; // Used to defer YYABORT in certain error cases
	// Extended Query specific fields
	PSQL_TypeOid *	 types;
	SqlStatementType command_tag;
	int16_t		 types_size;
	int32_t *	 parm_start; /* Note that the type size used for parm_start and parm_end is int32_t despite the fact that
				      *	only INT16_MAX parameters are possible. This is because this array is for tracking buffer
				      * offsets within the query string, whose length max exceed INT16_MAX since:
				      * OCTO_MAX_QUERY_LEN == YDB_MAX_STR == 1024 * 1024, while INT16_MAX == 32767.
				      */
	int32_t *parm_end;
	int16_t	 cur_param_num; /* used inside "populate_data_type" to fill in types of parameter placeholders in query
				 * based on context (e.g. binary operations where one operand type is known etc.).
				 */
	int16_t	   num_bind_parms;
	int16_t	   num_bind_parm_types;
	int16_t	   total_parms;
	boolean_t  is_extended_query;
	boolean_t  skip_cursor_cleanup;
	boolean_t *is_bind_parm; // Used to track which literal parameters are bind parameters
	int16_t	   is_bind_parm_size;
	char	   routine[MAX_ROUTINE_LEN];
} ParseContext;

typedef struct SqlDataTypeStruct {
	enum SqlDataType data_type;
	/* Below field is usable only in case "data_type" is the following types.
	 *	INTEGER_TYPE : stores the precision specified (e.g. 8 in INT(8))
	 *	NUMERIC_TYPE : stores the precision specified (e.g. 4 in NUMERIC(4,5))
	 *	STRING_TYPE  : stores the size specified      (e.g. 30 in VARCHAR(30))
	 * Is initialized to SIZE_OR_PRECISION_UNSPECIFIED otherwise.
	 * Note: For INTEGER_TYPE, even though we store the specified precision, it is actually not used.
	 * See comment under "integer_type_tail" rule in "src/parser.y" for more context.
	 */
	int size_or_precision;
	/* Below field is usable only in case "data_type" is the following type(s).
	 *	NUMERIC_TYPE : stores the scale specified (e.g. 5 in NUMERIC(4,5))
	 * Is initialized to SCALE_UNSPECIFIED otherwise.
	 */
	int scale;
	/* In some cases (e.g. type cast operator usages like "SELECT 1.50::NUMERIC(3,2);"), we would have created a
	 *	  parameter (using the INVOKE_PARSE_LITERAL_TO_PARAMETER macro) for the "size_or_precision" and "scale" fields.
	 *	  For example, using the "int_literal_value" rule in "src/parser.y". In that case, we need access to the
	 *	  parameter index for later use in "tmpl_print_expression()". Hence the below fields.
	 * In other cases (e.g. data_type specifications of columns in CREATE TABLE commands), we would not have created
	 *	a parameter as changes in the actual value of size/precision/scale needs to generate different plans.
	 *	In those cases the below parameter index fields are initialized to 0.
	 */
	int size_or_precision_parameter_index;
	int scale_parameter_index;
} SqlDataTypeStruct;

/**
 * Represents a SQL column; doubly linked list
 *
 * WARNING: in some cases, SqlColumnList is used instead of the linked list, namely when
 *  we are dealing with a SELECT column list because the column may be a calculated column
 */
typedef struct SqlColumn {
	struct SqlStatement *columnName; /* is non-NULL in most cases. Can be NULL in case this structure
					  * corresponds to a table-level constraint. It might be better to
					  * have a separate `boolean_t` field indicating whether a column
					  * is a constraint or not. Doesn't seem necessary at this point
					  * but might be something to consider for the future.
					  */
	struct SqlDataTypeStruct data_type_struct;
	int			 column_number;
	boolean_t		 is_hidden_keycol;
	struct SqlStatement *	 table;
	struct SqlStatement *	 delim;
	struct SqlStatement *	 keywords;
	void *			 bin_defn_offset; /* Refer to comments above similar field in SqlTable */
	dqcreate(SqlColumn);
} SqlColumn;

/*
 * GROUP BY related structure to store different GROUP BY information for various expression nodes in parse tree.
 * This information is used during qualification to perform GROUP BY expression matching and validation.
 * Also, its used to forward `group_by_column_num` of an expression to its corresponding logical plan in lp_generate_where().
 * This information is later used to emit GROUP BY related M code in physical plan.
 */
typedef struct group_by_fields_t {
	int group_by_column_num; /* Indicates which GROUP BY list node the SQL ELEMENT matches */
} group_by_fields_t;

typedef struct SqlColumnAlias {
	// SqlColumn or SqlColumnListAlias
	struct SqlStatement *column;
	// SqlTableAlias
	struct SqlStatement *table_alias_stmt; /* In case of SET Operation, this field will point to the SqlTableAlias
						* corresponding to the leftmost SELECT query in the SET operation.
						*/
	struct SqlStatement
	    *set_oper_stmt; /* This field is needed as `table_alias_stmt` does not give the entire picture for SET Operations
			     * whereas this lets one go through ALL SELECT queries involved in the SET Operation.
			     * Will be non-NULL only in case of a column alias corresponding to a SET operation.
			     */
	struct LogicalPlan *extract_lp; // A logical plan representing an `EXTRACT` DDL specification referencing a SQL function
	int		    group_by_column_number; /* 0 if this column name was not specified in a GROUP BY.
						     * Holds a non-zero index # if column name was specified in GROUP BY
						     * (e.g. in query `SELECT 1+id FROM names GROUP BY id,firstname`,
						     *  this field would be 1 for the SqlColumnAlias corresponding to
						     *  `id` and 2 for the SqlColumnAlias corresponding to `firstname`
						     *  and 0 for the SqlColumnAlias corresponding to `lastname`).
						     */
	void *bin_defn_offset;			    /* Refer to comments above similar field in SqlTable */
} SqlColumnAlias;

/*
 * Represents a SQL view
 */
typedef struct SqlView {
	struct SqlStatement *viewName;
	struct SqlStatement *src_table_alias_stmt;
	struct SqlStatement *column_name_list; /* This is only valid for a short duration between completion of parsing and
						* completion of view_definition() invocation in run_query(). When it is valid
						* it carries view column names that are explicitly specified in CREATE VIEW.
						* Once the invocation reaches view_definition() these column names are assigned
						* as alias to view definition's columns. Additionally, view_definition() invocation
						* is delayed till run_query() with the help of this variable to allow parser
						* to have enough context to recognize the SELECT_ASTERISK problem at parsing stage
						* itself. Refer to the following thread to know more about the SELECT_ASTERISK issue
						* https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1378#note_1380319421.
						* Note that SELECT_ASTERISK issue occurs in SELECT query, since view depends on
						* select queries to form its definition the issue had to be handled for view
						* as well.
						*/
} SqlView;
/*
 * Represents a SQL table
 */
typedef struct SqlTable {
	struct SqlStatement *tableName;
	struct SqlStatement *source;
	struct SqlStatement *columns; // SqlColumn
	struct SqlStatement *delim;
	struct SqlStatement *aim_type;	/* Type parameter for AIM */
	boolean_t	     readwrite; /* TRUE if READWRITE keyword is specified, FALSE if READONLY keyword is specified */
	uint64_t	     oid;	/* TABLEOID; compared against ^%ydboctoschema(TABLENAME,OCTOLIT_PG_CLASS) */
	boolean_t	     if_not_exists_specified;
	/* The following variable "bin_defn_offset" is present in all structures that can be part of a table/view/function
	 * binary definition and can be pointed to from multiple other structures (for example, multiple "SqlTableAlias" structures
	 * can point to the same "SqlTable" structure). This is used by compress_statement() and decompress_statement().
	 *
	 * 1) compress_statement() takes care of storing a runtime parse tree as a blob (binary object, aka binary definition,
	 *    which is an array of bytes) in the database.
	 * 2) decompress_statement() takes care of reading the blob from the database and converting it back into a runtime
	 *    parse tree.
	 * 3) After parsing a CREATE TABLE, CREATE VIEW or CREATE FUNCTION command, the parse tree would store an initial value
	 *    of 0 for "bin_defn_offset".
	 * 4) compress_statement() is called TWICE. Once to get the size of the blob. Second to do the actual blob conversion.
	 * 5) In the first compress_statement() call, the initial value of 0 of "bin_defn_offset" would be seen and this is
	 *    a signal to compress_statement() to do the size calculation of the structure holding this field. As part of that,
	 *    this field is set to the special value MAX_BIN_DEFN_OFFSET (which is an impossible offset, currently hardcoded to
	 *    (2^64)-1.
	 * 6) While still in the first compress_statement() call, any more function invocations that reach this structure
	 *    (possible due to multiple parse tree structures pointing to this same structure) will see the "bin_defn_offset"
	 *    set to MAX_BIN_DEFN_OFFSET in which case it will skip the size calculation for this structure as it has already
	 *    been done once.
	 * 7) In the second compress_statement() call, the special value of MAX_BIN_DEFN_OFFSET of "bin_defn_offset" would be
	 *    seen first in this structure and in this case it is a signal to compress_statement() to use up space in the
	 *    allocated blob to store the current structure. As part of that, "bin_defn_offset" is set to store the offset
	 *    into this allocated space in the blob.
	 * 8) While still in the second compress_statement() call, any more function invocations that reach this structure
	 *    would see the "bin_defn_offset" set to an offset (that is not MAX_BIN_DEFN_OFFSET) and so would set the
	 *    parent structure (that has a pointer to this structure) to store the "bin_defn_offset" field in the parent structure.
	 *    This way we allocate only one structure while allowing multiple parent structures to point to this structure.
	 * 9) decompress_statement() is called only ONCE. When it is called, on the first occurrence of a structure we see if
	 *    "bin_defn_offset" is non-NULL (which should be the case as we are seeing the current structure for the first time)
	 *    we set it to NULL and recurse through its various fields.
	 *10) When we again come across this structure again in decompress_statement(), the "bin_defn_offset" member will have a
	 *    NULL value indicating that this structure was already decompressed so nothing more is needed to be done.
	 */
	void *bin_defn_offset;
} SqlTable;

/* Below is the table constructed by the VALUES (...) syntax */
typedef struct SqlTableValue {
	struct SqlStatement *row_value_stmt; // SqlRowValue
	struct SqlStatement *column_stmt;    /* SqlColumn. Stored in "table_reference.c". Used in "populate_data_type.c" and
					      * hash_canonical_query.c
					      */
} SqlTableValue;

typedef struct SqlRowValue {
	int		     num_columns; /* number of columns in this row */
	struct SqlStatement *value_list;  /* list of values of all columns in this row (SqlColumnList) */
	dqcreate(SqlRowValue);		  /* doubly linked list pointer to next row in this table of values */
} SqlRowValue;

/* Various stages of qualify_query.
 * Currently only a few stages are needed. For example, there is no "QualifyQuery_WHERE" or "QualifyQuery_HAVING".
 * More can be added at a later stage as and when necessary.
 * `QualifyQuery_GROUP_BY_EXPRESSION` is used to inform qualify_statement() that an expression is being qualified
 * , do not update `group_by_column_number` and `group_by_column_count` for any COLUMN REFERENCE in the expression.
 */
typedef enum {
	QualifyQuery_NONE,
	QualifyQuery_SELECT_COLUMN_LIST,
	QualifyQuery_ORDER_BY,
	QualifyQuery_WHERE,
	QualifyQuery_GROUP_BY_EXPRESSION,
	QualifyQuery_UPDATE_SET_CLAUSE,
} QualifyQueryStage;

/* The below is used as a bitmask to form the "aggregate_function_or_group_by_or_having_specified" field below */
#define GROUP_BY_SPECIFIED	     0x1
#define HAVING_SPECIFIED	     0x2
#define AGGREGATE_FUNCTION_SPECIFIED 0x4

typedef struct SqlTableAlias {
	// SqlTable or SqlTableValue or SqlSelectStatement
	struct SqlStatement *table;
	// SqlValue
	struct SqlStatement *alias;
	int		     unique_id;
	// Below fields are used for GROUP BY validation and/or to track Aggregate function use
	int group_by_column_count;
	int aggregate_depth;					/* Non-zero and Positive if currently inside an aggregate function.
								 * Non-zero and Negative if currently inside a FROM or WHERE or
								 * GROUP BY clause. Used while qualifying a query for error
								 * checking in both the above cases.
								 */
	int aggregate_function_or_group_by_or_having_specified; /* bitmask of flags (e.g. GROUP_BY_SPECIFIED etc.)
								 * based on whether a GROUP BY, HAVING, and/or
								 * aggregate function were specified in the query.
								 */
	boolean_t do_group_by_checks; /* TRUE for the time we are in "qualify_statement()" while doing GROUP BY related checks
				       * in the HAVING, SELECT column list and ORDER BY clause. Note that "qualify_statement()"
				       * is invoked twice on these lists. The first time, this flag is FALSE. The second time
				       * it is TRUE. This is used to issue GROUP BY related errors (which requires us to have
				       * scanned the entire SELECT column list at least once). It is also used to avoid issuing
				       * duplicate errors (e.g. ERR_UNKNOWN_COLUMN_NAME).
				       */
	// SqlTableAlias
	struct SqlStatement *parent_table_alias;
	// SqlColumnListAlias list of available columns
	struct SqlStatement *column_list;
	/* `correlation_specification` is a pointer to a table name alias and an optional list of column name aliases.
	 * For example the following usage `AS tablealias(columnalias1, columnalias2)` in a query will lead to this field being
	 * set to a SQLColumnList with nodes corresponding to tablealias, columnalias1 and columnalias2 in the same order.
	 */
	struct SqlStatement *correlation_specification;
	// SqlColumnAlias
	struct SqlStatement *table_asterisk_column_alias; /* The ColumnAlias structure corresponding to a TABLE.* specification
							   * for this table. This is needed to ensure that all references to
							   * t1.* in the query (for table "t1") return a pointer to the same
							   * SqlColumnAlias structure as otherwise GROUP BY validation would fail
							   * because "group_by_column_number" can only be maintained in one such
							   * column alias and not multiple copies of it.
							   */
	QualifyQueryStage qualify_query_stage;
	void *		  bin_defn_offset; /* Refer to comments above similar field in SqlTable */
} SqlTableAlias;

/**
 * Represents an optional KEYWORD which has a value associated with it */
typedef struct SqlOptionalKeyword {
	enum OptionalKeyword keyword;
	// Keyword value (SqlValue) or UNION statement (SqlSelectStatement) or CONSTRAINT (SqlConstraint)
	struct SqlStatement *v;
	dqcreate(SqlOptionalKeyword);
} SqlOptionalKeyword;

// Stores custom index information specified in INDEX statements.
typedef struct SqlIndex {
	struct SqlStatement *indexName;
} SqlIndex;

typedef struct SqlDelimiterCharacterList {
	// struct SqlStatement	*character;	// SqlValue
	int character;
	dqcreate(SqlDelimiterCharacterList);
} SqlDelimiterCharacterList;

/**
 * Effectively provides a list of tables that may or may not be joined
 */
typedef struct SqlJoin {
	// SqlTableAlias
	//  -> was SqlTable, should be changed everywhere
	struct SqlStatement *value;
	// SqlValue
	struct SqlStatement *condition;
	enum SqlJoinType     type;
	/* The below field stores the value of "config->plan_id" at the time this join was parsed in the query.
	 * That gives us a picture of the maximum number of tables (and in turn table_alias->unique_id) that
	 * were processed till now. To elaborate, since "config->plan_id" is a constantly increasing id that we
	 * assign to tables encountered in the query as we parse it, this one number is a good idea of all table
	 * ids that are valid as of the current JOIN. Any id that is greater than this is guaranteed to have been
	 * encountered AFTER this JOIN and so any column reference corresponding to a table with that id cannot be
	 * safely moved from the WHERE clause (where all column references are valid) to this JOIN ON clause (where
	 * this column reference is not valid).
	 */
	int max_unique_id;
	dqcreate(SqlJoin);
} SqlJoin;

/**
 * Represents a SQL SELECT statement
 */
typedef struct SqlSelectStatement {
	// SqlColumnListAlias
	struct SqlStatement *select_list;
	// SqlJoin
	struct SqlStatement *table_list;
	// SqlValue (?)
	struct SqlStatement *where_expression;
	// SqlColumnListAlias
	struct SqlStatement *group_by_expression;
	// SqlValue (?)
	struct SqlStatement *having_expression;
	// SqlValue (?)
	struct SqlStatement *order_by_expression;
	// SqlOptionalKeyword
	struct SqlStatement *optional_words;
} SqlSelectStatement;

typedef struct SqlInsertStatement {
	struct SqlStatement *dst_table_alias_stmt; /* SqlTableAlias */
	struct SqlStatement *columns;		   /* SqlColumnList */
	struct SqlStatement *src_table_alias_stmt; /* SqlTableAlias */
	struct SqlStatement *optional_words;	   /* At present this field will be either NULL or
						    * SqlOptionalKeyword of type OVERRIDE_SYSTEM_VALUE or
						    * SqlOptionalKeyword of type OVERRIDE_USER_VALUE
						    */
} SqlInsertStatement;

typedef struct SqlDeleteFromStatement {
	struct SqlStatement *src_join;	   /* SqlJoin */
	struct SqlStatement *where_clause; /* SqlBinaryOperation or SqlUnaryOperation etc. */
} SqlDeleteFromStatement;

typedef struct SqlUpdateColumnValue {
	struct SqlStatement *col_name;	/* Contains name of the column (SqlValue) at start of parsing.
					 * "update_statement.c" later modifies this to contain SqlColumn.
					 */
	struct SqlStatement *col_value; /* Value to assign to the column. Can be SqlOptionalKeyword -> DEFAULT,
					 * SqlValue, SqlBinaryOperation, etc.
					 */
	dqcreate(SqlUpdateColumnValue);
} SqlUpdateColumnValue;

typedef struct SqlUpdateStatement {
	struct SqlStatement * src_join; /* SqlJoin */
	SqlUpdateColumnValue *col_value_list;
	struct SqlStatement * where_clause; /* SqlBinaryOperation or SqlUnaryOperation etc. */
} SqlUpdateStatement;

typedef struct SqlDropTableStatement {
	// SqlValue
	struct SqlStatement *table_name;
	// SqlOptionalKeyword
	struct SqlStatement *optional_keyword;
	enum OptionalKeyword drop_data_retention;
	boolean_t	     if_exists_specified;
} SqlDropTableStatement;

typedef struct SqlDropViewStatement {
	// SqlValue
	struct SqlStatement *view_name;
	boolean_t	     if_exists_specified;
} SqlDropViewStatement;

typedef struct SqlTruncateTableStatement {
	// SqlValue
	struct SqlStatement *tables; // SqlColumnListAlias
} SqlTruncateTableStatement;

typedef struct SqlArray {
	struct SqlStatement *argument;
} SqlArray;

/*
 * Represents a unary operation
 */
typedef struct SqlUnaryOperation {
	enum UnaryOperations operation; // '+', '-'
	struct SqlStatement *operand;
	group_by_fields_t    group_by_fields;
} SqlUnaryOperation;

/*
 * Represents an arithmetic operation
 */
typedef struct SqlBinaryOperation {
	enum BinaryOperations operation; // '+', '-', '*', '/'
	struct SqlStatement * operands[2];
	group_by_fields_t     group_by_fields;
} SqlBinaryOperation;

typedef struct SqlAggregateFunction {
	SqlAggregateType type;	     // COUNT_ASTERISK, AVG, SUM, MIN, MAX
	SqlValueType	 param_type; /* Data type (STRING_LITERAL, NUMERIC_LITERAL etc.) of function parameter.
				      * Initialized/Needed only if `type` is AGGREGATE_MIN or AGGREGATE_MAX.
				      */
	// SqlColumnList
	struct SqlStatement *parameter;
	struct SqlStatement *table_alias_stmt; /* If not NULL the unique_id of this table_alias will be copied to `unique_id` of
						* LpExtraAggregateFunction in lp_generate_where(), which later is referred by
						* lp_verify_structure() to attach this aggregate to the correct physical plan during
						* physical plan generation.
						*/
} SqlAggregateFunction;

typedef struct SqlFunctionCall {
	// SqlValue
	struct SqlStatement *function_name;
	// SqlFunction
	struct SqlStatement *function_schema;
	// SqlColumnList
	struct SqlStatement *parameters;
} SqlFunctionCall;

typedef struct SqlCoalesceCall {
	// SqlColumnList
	struct SqlStatement *arguments;
} SqlCoalesceCall;

typedef struct SqlGreatest {
	// SqlColumnList
	struct SqlStatement *arguments;
} SqlGreatest;

typedef struct SqlLeast {
	// SqlColumnList
	struct SqlStatement *arguments;
} SqlLeast;

typedef struct SqlNullIf {
	struct SqlStatement *left, *right;
} SqlNullIf;

/**
 * Represents a SQL function
 */
typedef struct SqlFunction {
	struct SqlStatement *function_name;	  // SqlValue
	struct SqlStatement *parameter_type_list; // SqlParameterTypeList
	struct SqlStatement *return_type;	  // SqlDataTypeStruct
	struct SqlStatement *extrinsic_function;  // SqlValue
	struct SqlStatement *function_hash;	  // SqlValue
	int32_t		     num_args;
	uint64_t	     oid;
	boolean_t	     if_not_exists_specified;
} SqlFunction;

typedef struct SqlDropFunctionStatement {
	struct SqlStatement *function_name;	  // SqlValue
	struct SqlStatement *parameter_type_list; // SqlParameterTypeList
	boolean_t	     if_exists_specified;
} SqlDropFunctionStatement;

typedef struct SqlParameterTypeList {
	struct SqlStatement *data_type_struct; // SqlDataTypeStruct
	dqcreate(SqlParameterTypeList);
} SqlParameterTypeList;

typedef struct SqlValue {
	enum SqlValueType type;
	union {
		struct {
			SqlDataTypeStruct coerced_type;	    /* initialized/usable only if `type` is COERCE_TYPE */
			enum SqlValueType pre_coerced_type; /* initialized/usable only if `type` is COERCE_TYPE */
		} coerce_type;
		union {
			boolean_t truth_value; /* initialized/usable only if `type` is BOOLEAN_OR_STRING_LITERAL */
		} bool_or_str;
	} u;
	group_by_fields_t group_by_fields; /* Used in case of COERCE_TYPE and CALCULATED_VALUE */
	int		  parameter_index;
	boolean_t	  is_double_quoted;
	union {
		char *string_literal;
		char *reference;
		// SqlBinaryOperation, SqlUnaryOperation, SqlFunctionCall, SqlAggregateFunction
		struct SqlStatement *calculated;
		// Target to coerce; SqlValue, SqlColumnAlias
		struct SqlStatement *coerce_target;
	} v;
} SqlValue;

/**
 * Used to represent a SELECT column list, not a table column list.
 *
 * Note that this structure is used for storing SQL function parameters as well,
 * so any types stored in the `value` member must be accounted for in the
 * `function_call_STATEMENT` branch of `populate_data_type`.
 */
typedef struct SqlColumnList {
	struct SqlStatement *value;			     /* SqlValue, SqlColumnAlias, SqlUnaryOperation or SqlColumn */
	uint64_t	     qualify_extract_function_cycle; /* Track recursion/dependency cycles in EXTRACT function argument lists
							      */
	dqcreate(SqlColumnList);
} SqlColumnList;

// Structure to hold a table unique_id and a column number. Used in SqlColumnListAlias for ORDER BY columns
// that match an alias name of a column in the SELECT column list.
typedef struct SqlTableIdColumnId {
	int unique_id;
	int column_number;
} SqlTableIdColumnId;

typedef struct SqlColumnListAlias {
	// SqlColumnList
	struct SqlStatement *column_list;
	// SqlValue
	struct SqlStatement *alias;
	// Keywords used for the SORT column
	struct SqlStatement *keywords;
	SqlValueType	     type;
	boolean_t	     user_specified_alias;
	/* The below field is needed to store the unique_id of the table and column number of the column in the
	 * SELECT column list of that table if this cla was matched to an ALIAS NAME from another cla
	 * (see "QUALIFY_COLUMN_REFERENCE" in qualify_statement.c).
	 */
	SqlTableIdColumnId	   tbl_and_col_id;
	struct SqlColumnListAlias *duplicate_of_column; /* NULL mostly. If non-NULL (possible only in case of a
							 * NATURAL JOIN), this points to the column from a preceding
							 * table in the join list with the same name as this column.
							 */
	// SqlColumnAlias
	struct SqlStatement *outer_query_column_alias; // the ColumnAlias structure corresponding to this
						       // ColumnListAlias if/when referenced in outer query
	void *bin_defn_offset;			       /* Refer to comments above similar field in SqlTable */
	dqcreate(SqlColumnListAlias);
} SqlColumnListAlias;

/*
 * A SQL set operation, such as UNION, EXCEPT, or INTERSECT
 *
 * This is separated from a binary operation because, at this time, I believe it
 *  to be not useable as part of an expression; if this proves to be wrong,
 *  we should consider merging it
 */
typedef struct SqlSetOperation {
	SqlSetOperationType  type;
	struct SqlStatement *operand[2];
	struct SqlStatement *col_type_list_stmt; /* SqlColumnListAlias. List of available columns with type information indicating
						  * the union of the types of the two operands of the SET operation. For example if
						  * this is an INTERSECT SET operation and the left operand has a column of type
						  * NUL_VALUE and the right operand has the same column of type INTEGER_LITERAL,
						  * then the SET operation would store INTEGER_LITERAL as the type (since NUL_VALUE
						  * can be matched with any other type, the other type should be inherited as the
						  * type of this column as the result of this SET operation). Used only by
						  * `populate_data_type` for type check of columns involved in the SET operation.
						  */
} SqlSetOperation;

typedef struct SqlBeginStatement {
	// Filler so compiler doesn't complain about empty type;
	// when we add something to this struct, simply replace this filler
	char b;
} SqlBeginStatement;

typedef struct SqlCommitStatement {
	// Filler so compiler doesn't complain about empty type;
	// when we add something to this struct, simply replace this filler
	char b;
} SqlCommitStatement;

typedef struct SqlCaseStatement {
	// SqlValue
	struct SqlStatement *value;
	// SqlCaseBranchStatement
	struct SqlStatement *branches;
	// SqlValue
	struct SqlStatement *optional_else;
	group_by_fields_t    group_by_fields;
} SqlCaseStatement;

typedef struct SqlCaseBranchStatement {
	// SqlValue
	struct SqlStatement *condition;
	// SqlValue
	struct SqlStatement *value;
	dqcreate(SqlCaseBranchStatement);
} SqlCaseBranchStatement;

typedef struct SqlSetStatement {
	struct SqlStatement *variable;
	struct SqlStatement *value;
} SqlSetStatement;

typedef struct SqlShowStatement {
	struct SqlStatement *variable;
} SqlShowStatement;

typedef struct SqlNoDataStatement {
	// Filler so compiler doesn't complain about empty type;
	// when we add something to this struct, simply replace this filler
	char b;
} SqlNoDataStatement;

typedef struct SqlConstraint {
	enum OptionalKeyword type;	 /* The constraint type. Current valid values are
					  *     PRIMARY_KEY
					  *	UNIQUE_CONSTRAINT
					  *	OPTIONAL_CHECK_CONSTRAINT
					  */
	struct SqlStatement *name;	 /* Name of constraint (user specified or automatically assigned) */
	struct SqlStatement *definition; /* Constraint specific content.
					  * For OPTIONAL_CHECK_CONSTRAINT, this will point to the CHECK condition
					  *      that needs to be satisfied.
					  * For UNIQUE_CONSTRAINT, this will point to the list of column names that
					  *	 comprise the UNIQUE constraint.
					  */
	union {
		struct SqlStatement *check_columns; /* Currently non-NULL only for a OPTIONAL_CHECK_CONSTRAINT. In that case
						     * it points to a "SqlColumnList". This is the list of names of columns
						     * referenced in this CHECK constraint.
						     */
		struct SqlStatement *uniq_gblname;  /* Non-NULL only for a UNIQUE_CONSTRAINT. In that case it points to a
						     * "SqlValue" that holds the global variable name which helps enforce
						     * the UNIQUE constraint.
						     */
	} v;
} SqlConstraint;

typedef struct SqlDisplayRelation {
	SqlDisplayRelationType type;
	struct SqlStatement *  table_name; /* Used by `\d tablename` command */
} SqlDisplayRelation;

typedef struct SqlStatement {
	enum SqlStatementType type;
	struct YYLTYPE	      loc;
	union {
		/* Note: The order of the fields listed below is the same as the order of statement types listed in
		 * "typedef enum SqlStatementType" in a different section of this same file ("octo_types.h").
		 */
		struct SqlTable *		  create_table;
		struct SqlView *		  create_view;
		struct SqlFunction *		  create_function;
		struct SqlSelectStatement *	  select;
		struct SqlInsertStatement *	  insert;
		struct SqlDropTableStatement *	  drop_table;
		struct SqlDropViewStatement *	  drop_view;
		struct SqlDropFunctionStatement * drop_function;
		struct SqlTruncateTableStatement *truncate_table;
		struct SqlValue *		  value;
		struct SqlFunctionCall *	  function_call;
		struct SqlCoalesceCall *	  coalesce;
		struct SqlGreatest *		  greatest;
		struct SqlLeast *		  least;
		struct SqlNullIf *		  null_if;
		struct SqlAggregateFunction *	  aggregate_function;
		struct SqlBinaryOperation *	  binary;
		struct SqlUnaryOperation *	  unary;
		struct SqlColumnList *		  column_list;
		struct SqlColumn *		  column; // Note singular versus plural
		struct SqlJoin *		  join;
		struct SqlParameterTypeList *	  parameter_type_list;
		struct SqlConstraint *		  constraint; /* corresponding to constraint_STATEMENT */
		struct SqlOptionalKeyword *	  keyword;
		struct SqlColumnListAlias *	  column_list_alias;
		struct SqlColumnAlias *		  column_alias;
		struct SqlTableAlias *		  table_alias;
		struct SqlSetOperation *	  set_operation;
		struct SqlBeginStatement *	  begin;
		struct SqlCommitStatement *	  commit;
		struct SqlCaseStatement *	  cas;
		struct SqlCaseBranchStatement *	  cas_branch;
		struct SqlSetStatement *	  set;
		struct SqlShowStatement *	  show;
		struct SqlNoDataStatement *	  no_data;
		struct SqlDelimiterCharacterList *delim_char_list;
		struct SqlIndex *		  index;
		struct SqlDataTypeStruct	  data_type_struct;
		struct SqlDisplayRelation *	  display_relation;
		enum SqlJoinType		  join_type;
		/* Below SqlStatementType types do not have any parameters so they do not have corresponding members here.
		 *	discard_all_STATEMENT
		 */
		struct SqlRowValue *  row_value;   /* corresponding to row_value_STATEMENT */
		struct SqlTableValue *table_value; /* corresponding to table_value_STATEMENT */
		struct SqlArray *     array;
		/* Below SqlStatementType types do not have any parameters so they do not have corresponding members here.
		 *	history_STATEMENT
		 */
		struct SqlDeleteFromStatement *delete_from;
		struct SqlUpdateStatement *    update;
		/* Below SqlStatementType types do not have any parameters so they do not have corresponding members here.
		 *	invalid_STATEMENT
		 */
	} v;
	/* The below is used during "hash_canonical_query" to avoid multiple traversals of same node.
	 * Before "hash_canonical_query()" is reached, this field is used to store a 4-byte "max_unique_id"
	 * if this SqlStatement structure falls inside the subtree of a WHERE or ON clause. In that case, it
	 * represents the MAXIMUM value of "table_alias->unique_id" where "table_alias" is the table alias corresponding
	 * to any/all column references inside that subtree. For example if the WHERE clause had "(n1.id = 3)" then
	 * this field will represent the "unique_id" corresponding to the table_alias for "n1". This is used to compare
	 * against "SqlJoin.max_unique_id" field to see if a subtree of the WHERE clause can be moved over to the
	 * ON clause of the JOIN.
	 */
	uint64_t hash_canonical_query_cycle;
} SqlStatement;

/* The below is used by qualify_statement.c */
typedef struct {
	SqlColumnListAlias **ret_cla;
	int *		     max_unique_id;
	SqlStatement *	     deepest_column_alias_stmt; /* Used by COLUMN_REFERENCE case of qualify_statement() to communicate
							 * the deepest `column_alias` in the aggregate parameter to
							 * aggregate_function_STATEMENT case.
							 */
	struct SqlStatement
	    *aggr_table_alias_stmt; /* Used by aggregate_function_STATEMENT case to convey the `unique_id` of the deepest
				     * `column_alias` present in it to column_alias_STATEMENT case of qualify_statement() so that
				     * GROUP BY validations can be performed on the column reference present in aggregate function.
				     * This value is only valid for the duration of time qualify_statement is processing an
				     * aggregate and its parameter.
				     */
} QualifyStatementParms;
#endif
