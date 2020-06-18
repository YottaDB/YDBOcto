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

#ifndef OCTO_TYPES_H
#define OCTO_TYPES_H

#include <stdint.h>	/* needed for uint64_t */

typedef void *yyscan_t;

#include <libyottadb.h>

#include "memory_chunk.h"
#include "double_list.h"

// Set maximum M routine length - must be in sync with MAX_MIDENT_LEN in YDB/sr_port/mdef.h
#define MAX_ROUTINE_LEN YDB_MAX_IDENT

// Set maximum command tag length for use in extended query protocol, including null terminator
// This value should be large enough to hold the longest possible first keyword of a SQL query, i.e. "DEALLOCATE"
#define MAX_TAG_LEN 11

// Per https://www.postgresql.org/docs/11/catalog-pg-type.html, for type length values of -1 and -2:
//	"-1 indicates a 'varlena' type (one that has a length word), -2 indicates a null-terminated C string."
#define TYPLEN_VARLENA		-1
#define TYPLEN_CSTRING		-2

// Allocates ONE structure of type TYPE
#define	OCTO_CMALLOC_STRUCT(RET, TYPE)								\
{												\
	RET = (TYPE *)octo_cmalloc(memory_chunks, sizeof(TYPE));				\
}

#define SQL_STATEMENT(VAR, TYPE)			      					\
{												\
	OCTO_CMALLOC_STRUCT(VAR, SqlStatement);							\
	(VAR)->type = TYPE;									\
}

#define MALLOC_STATEMENT(VAR, NAME, TYPE)							\
{												\
	OCTO_CMALLOC_STRUCT((VAR)->v.NAME, TYPE);						\
}

#define UNPACK_SQL_STATEMENT(result, item, StatementType)					\
{												\
	assert((item)->type == StatementType ## _STATEMENT);					\
	(result) = (item)->v.StatementType;							\
}

#define PACK_SQL_STATEMENT(out, item, StatementType)						\
{												\
	SQL_STATEMENT(out, StatementType ## _STATEMENT);					\
	(out)->v.StatementType = item;								\
}

#define SHALLOW_COPY_SQL_STATEMENT(dst, src, NAME, TYPE) do {					\
	SQL_STATEMENT((dst), src->type);							\
	MALLOC_STATEMENT((dst), NAME, TYPE);							\
	*(dst)->v.NAME = *(src)->v.NAME;							\
} while (0);

/* Determines the corresponding (SqlStatement *) structures that points to a (SqlTable *) structure */
#define	SQL_STATEMENT_FROM_TABLE_STATEMENT(RET, TABLE)			\
{									\
	RET = (SqlStatement *)((char *)TABLE - sizeof(SqlStatement));	\
	assert(create_table_STATEMENT == RET->type);			\
	assert(RET->v.create_table == TABLE);				\
}

/* Shamelessly stolen from mlkdef.h in YottaDB */
/* convert relative pointer to absolute pointer */
#define R2A(X) (void*)(((unsigned char*) &(X)) + ((size_t)X))

/* store absolute pointer Y in X as a relative pointer */
#define A2R(X, Y) ((X) = (void*)(((unsigned char*)(Y)) - ((unsigned char*) &(X))))

#define	IS_OUTER_JOIN(JOIN_TYPE)	((LEFT_JOIN == JOIN_TYPE) || (RIGHT_JOIN == JOIN_TYPE) || (FULL_JOIN == JOIN_TYPE))

typedef long long unsigned int uint8;

typedef int boolean_t;

typedef enum FileType {
	CrossReference,
	OutputPlan,
	YDBTrigger,
} FileType;

typedef enum SqlStatementType {
	create_table_STATEMENT,
	create_function_STATEMENT,
	select_STATEMENT,
	insert_STATEMENT,
	drop_table_STATEMENT,
	drop_function_STATEMENT,
	value_STATEMENT,
	function_call_STATEMENT,
	aggregate_function_STATEMENT,
	binary_STATEMENT,
	unary_STATEMENT,
	column_list_STATEMENT,
	column_STATEMENT,
	join_STATEMENT,
	data_type_STATEMENT,
	parameter_type_list_STATEMENT,
	constraint_STATEMENT,
	constraint_type_STATEMENT,
	keyword_STATEMENT,
	column_list_alias_STATEMENT,
	column_alias_STATEMENT,
	table_alias_STATEMENT,
	join_type_STATEMENT,
	set_operation_STATEMENT,
	begin_STATEMENT,
	commit_STATEMENT,
	cas_STATEMENT,
	cas_branch_STATEMENT,
	set_STATEMENT,
	show_STATEMENT,
	no_data_STATEMENT,
	sort_spec_list_STATEMENT,
	delim_char_list_STATEMENT,
	index_STATEMENT,
	invalid_STATEMENT,
} SqlStatementType;

// The order of these must be kept in sync with `LPActionType` in `src/optimization_transforms/lp_action_type.hd`
typedef enum UnaryOperations {
	FORCE_NUM,
	NEGATIVE,
	BOOLEAN_NOT,
	BOOLEAN_EXISTS,
	BOOLEAN_NOT_EXISTS,	// Not used but needed to be in sync with LP_BOOLEAN_NOT_EXISTS in `lp_action_type.hd`
	BOOLEAN_IS_NULL,
	BOOLEAN_IS_NOT_NULL,
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
	EXTRINSIC_FUNCTION_NAME,
	PARAMETER_VALUE,
	NUL_VALUE,
	COERCE_TYPE,
	DELIM_VALUE,
	INVALID_SqlValueType
} SqlValueType;

typedef enum SqlDataType {
	UNKNOWN_SqlDataType,
	BOOLEAN_TYPE,
	INTEGER_TYPE,
	NUMERIC_TYPE,
	STRING_TYPE
} SqlDataType;

/* Note: Additions of keywords in the middle of the table can cause SIG-11s because the actual binary value
 *       of these enums (e.g. PRIMARY_KEY) is stored in the ^%ydboctoschema(<tablename>,"b",*) global nodes
 *       and using a newer build of Octo without killing ^%ydboctoschema could load a table definition that
 *       is out of date with respect to the newer build.
 * Note: Any additions/deletions to this list might need to be correspondingly changed in "lp_emit_plan.c".
 */
typedef enum OptionalKeyword {
	NO_KEYWORD,
	PRIMARY_KEY,
	NOT_NULL,
	UNIQUE_CONSTRAINT,
	OPTIONAL_SOURCE,
	OPTIONAL_END,
	OPTIONAL_START,
	OPTIONAL_DELIM,
	OPTIONAL_EXTRACT,
	OPTIONAL_CASCADE,
	OPTIONAL_RESTRICT,
	OPTIONAL_PIECE,
	OPTIONAL_KEY_NUM,
	OPTIONAL_ADVANCE,
	OPTIONAL_LIMIT,
	OPTIONAL_DISTINCT,
	OPTIONAL_XREF_INDEX,		// not sure if this should be here; gets populated through LP
	OPTIONAL_BOOLEAN_EXPANSION,	// indicates that this statement is part of an OR boolean expression expansion to BNF form
	OPTIONAL_ASC,
	OPTIONAL_DESC,
} OptionalKeyword;

typedef enum SqlSetOperationType {
	SET_UNION,
	SET_UNION_ALL,
	SET_EXCEPT,
	SET_EXCEPT_ALL,
	SET_INTERSECT,
	SET_INTERSECT_ALL
} SqlSetOperationType;

typedef enum SqlJoinType {
	NO_JOIN,
	TABLE_SPEC,
	CROSS_JOIN,
	INNER_JOIN,
	RIGHT_JOIN,
	LEFT_JOIN,
	FULL_JOIN,
	NATURAL_JOIN
} SqlJoinType;

/* Note: Order of the below enums should be kept in sync with order of `LP_AGGREGATE_FUNCTION_*` types in `lp_action_type.hd` */
typedef enum SqlAggregateType {
	COUNT_ASTERISK_AGGREGATE,
	COUNT_AGGREGATE,
	SUM_AGGREGATE,
	AVG_AGGREGATE,
	MIN_AGGREGATE,
	MAX_AGGREGATE,
	COUNT_AGGREGATE_DISTINCT,
	SUM_AGGREGATE_DISTINCT,
	AVG_AGGREGATE_DISTINCT,
	/* Note: MIN_AGGREGATE_DISTINCT is equivalent to MIN_AGGREGATE */
	/* Note: MAX_AGGREGATE_DISTINCT is equivalent to MAX_AGGREGATE */
	AGGREGATE_LAST
} SqlAggregateType;

// Values for this enum are derived from the PostgreSQL catalog and
// only include types Octo currently supports.
// Typename to OID mappings can be acquired by running the following
// query against an existing PostgreSQL database:
//	select typname,oid from pg_catalog.pg_type
typedef enum {
	PSQL_TypeOid_bool = 16,
	PSQL_TypeOid_int4 = 23,
	PSQL_TypeOid_unknown = 705,
	PSQL_TypeOid_varchar = 1043,
	PSQL_TypeOid_numeric = 1700,
} PSQL_TypeOid;

// Values for this enum are derived from the PostgreSQL catalog and
// only include types Octo currently supports.
// Typename to type size mappings can be acquired by running the following
// query against an existing PostgreSQL database:
//	select oid,typname,typlen from pg_catalog.pg_type
typedef enum {
	PSQL_TypeSize_unknown = TYPLEN_CSTRING,
	PSQL_TypeSize_numeric = TYPLEN_VARLENA,
	PSQL_TypeSize_varchar = TYPLEN_VARLENA,
	PSQL_TypeSize_bool = 1,
	PSQL_TypeSize_int4 = 4,
} PSQL_TypeSize;

// Simple enum for distinguishing between different types of schemas when caching
typedef enum {
	TableSchema,
	FunctionSchema,
} SqlSchemaType;

#define YYLTYPE yyltype

typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
	int first_line;
	int first_column;
	int last_line;
	int last_column;
};

// Used to maintain various parse related information primarily for use in Extended Query protocol modules
typedef struct {
	// General purpose parser fields
	ydb_long_t		cursorId;
	char			*cursorIdString;
	boolean_t		abort;				// Used to defer YYABORT in certain error cases
	// Extended Query specific fields
	PSQL_TypeOid		*types;
	SqlStatementType	command_tag;
	int16_t			types_size;
	int16_t			*parm_start;
	int16_t			*parm_end;
	int16_t			cur_type;
	int16_t			num_bind_parms;
	int16_t			num_bind_parm_types;
	int16_t			total_parms;
	boolean_t		is_select;
	boolean_t		is_extended_query;
	boolean_t		skip_cursor_cleanup;
	boolean_t		*is_bind_parm;			// Used to track which literal parameters are bind parameters
	int16_t			is_bind_parm_size;
	char			routine[MAX_ROUTINE_LEN];
} ParseContext;

struct SqlColumn;
struct SqlColumnAlias;
//struct SqlConstraint;
struct SqlSelectStatement;
struct SqlInsertStatement;
struct SqlDropTableStatement;
struct SqlUnaryOperation;
struct SqlBinaryOperation;
struct SqlFunctionCall;
struct SqlValue;
struct SqlColumnList;
struct SqlTable;
struct SqlFunction;
struct SqlDropFunctionStatement;
struct SqlTableAlias;
struct SqlJoin;
struct SqlColumnListAlias;
struct SqlStatement;
struct SqlSetOperation;
struct SqlBeginStatement;
struct SqlCommitStatement;
struct SqlCaseStatement;
struct SqlCaseBranchStatement;
struct SqlSetStatement;
struct SqlShowStatement;
struct SqlNoDataStatement;

/**
 * Represents a SQL column; doubly linked list
 *
 * WARNING: in some cases, SqlColumnList is used instead of the linked list, namely when
 *  we are dealing with a SELECT column list because the column may be a calculated column
 */
typedef struct SqlColumn
{
	struct SqlStatement		*columnName;
	enum SqlDataType		type;
	int				column_number;
	struct SqlStatement		*table;
	struct SqlStatement		*keywords;
	/* Below field ("pre_qualified_cla") is initialized/usable only if "type" field above is UNKNOWN_SqlDataType.
	 * It is needed after parsing starts to handle a column that came in from a sub-query before when the sub-query
	 * column name was qualified (in "qualify_statement()"). Once the column names have been qualified,
	 * "populate_data_type()" relies on this field to derive the type of this SqlColumn structure
	 * (i.e. outer-query column name) based on the type that was determined for "pre_qualified_cla"
	 * (i.e. the sub-query column) in "qualify_statement()".
	 */
	struct SqlColumnListAlias	*pre_qualified_cla; /* initialized/usable only if "type" field is UNKNOWN_SqlDataType */
	dqcreate(SqlColumn);
} SqlColumn;

typedef struct SqlColumnAlias
{
	// SqlColumn or SqlColumnListAlias
	struct SqlStatement	*column;
	// SqlTableAlias
	struct SqlStatement	*table_alias_stmt;
	int			group_by_column_number;		/* 0 if this column name was not specified in a GROUP BY.
								 * Holds a non-zero index # if column name was specified in GROUP BY
								 * (e.g. in query `SELECT 1+id FROM names GROUP BY id,firstname`,
								 *  this field would be 1 for the SqlColumnAlias corresponding to
								 *  `id` and 2 for the SqlColumnAlias corresponding to `firstname`
								 *  and 0 for the SqlColumnAlias corresponding to `lastname`).
								 */
} SqlColumnAlias;

/**
 * Represents a SQL table
 */
typedef struct SqlTable
{
	struct SqlStatement	*tableName;
	struct SqlStatement	*source;
	struct SqlStatement	*columns;
	struct SqlStatement	*delim;
	uint64_t		oid;		/* TABLEOID; compared against ^%ydboctoschema(TABLENAME,"pg_class") */
} SqlTable;

typedef struct SqlTableAlias
{
	// SqlTable or SqlSelectStatement
	struct SqlStatement		*table;
	// SqlValue
	struct SqlStatement		*alias;
	int				unique_id;
	// Below fields are used for GROUP BY validation and/or to track Aggregate function use
	int				group_by_column_count;
	int				aggregate_depth;
	boolean_t			aggregate_function_or_group_by_specified;
	boolean_t			do_group_by_checks;	/* TRUE for the time we are in "qualify_statement()" while doing
								 * GROUP BY related checks in the SELECT column list and ORDER BY
								 * list. Note that "qualify_statement()" is invoked twice on these
								 * lists. The first time, this flag is FALSE. The second time it is
								 * TRUE. This is used to issue GROUP BY related errors (which
								 * requires us to have scanned the entire SELECT column list at
								 * least once) and also to avoid issuing duplicate errors related
								 * to Unknown column name etc.
								 */
	struct SqlTableAlias		*parent_table_alias;
	// SqlColumnListAlias list of available columns
	struct SqlStatement		*column_list;
} SqlTableAlias;

/**
 * Represents an optional KEYWORD which has a value associated with it */
typedef struct SqlOptionalKeyword
{
	enum OptionalKeyword keyword;
	// Keyword value (SqlValue) or UNION statement (SqlSelectStatement)
	struct SqlStatement *v;
	dqcreate(SqlOptionalKeyword);
} SqlOptionalKeyword;

// Stores custom index information specified in INDEX statements.
typedef struct SqlIndex
{
	struct SqlStatement *indexName;
} SqlIndex;

typedef struct SqlDelimiterCharacterList {
	// struct SqlStatement	*character;	// SqlValue
	int	character;
	dqcreate(SqlDelimiterCharacterList);
} SqlDelimiterCharacterList;

/**
 * Effectively provides a list of tables that may or may not be joined
 */
typedef struct SqlJoin
{
	// SqlTableAlias
	//  -> was SqlTable, should be changed everywhere
	struct SqlStatement *value;
	// SqlValue
	struct SqlStatement *condition;
	dqcreate(SqlJoin);
	enum SqlJoinType type;
} SqlJoin;

/**
 * Represents a SQL SELECT statement
 */
typedef struct SqlSelectStatement
{
	// SqlColumnListAlias
	struct SqlStatement	*select_list;
	// SqlJoin
	struct SqlStatement	*table_list;
	// SqlValue (?)
	struct SqlStatement	*where_expression;
	// SqlColumnListAlias
	struct SqlStatement	*group_by_expression;
	// SqlValue (?)
	struct SqlStatement	*having_expression;
	// SqlValue (?)
	struct SqlStatement	*order_by_expression;
	// SqlOptionalKeyword
	struct SqlStatement	*optional_words;
} SqlSelectStatement;

typedef struct SqlInsertStatement
{
	SqlTable *destination;
	struct SqlStatement *source;
	struct SqlStatement *columns;
} SqlInsertStatement;

typedef struct SqlDropTableStatement
{
	// SqlValue
	struct SqlStatement *table_name;
	// SqlOptionalKeyword
	struct SqlStatement *optional_keyword;
} SqlDropTableStatement;

/*
 * Represents an binary operation
 */
typedef struct SqlUnaryOperation
{
	enum UnaryOperations operation; // '+', '-'
	struct SqlStatement *operand;
} SqlUnaryOperation;

/*
 * Represents an arithmetic operation
 */
typedef struct SqlBinaryOperation
{
	enum BinaryOperations operation; // '+', '-', '*', '/'
	struct SqlStatement *operands[2];
} SqlBinaryOperation;

typedef struct SqlAggregateFunction {
	SqlAggregateType	type;			// COUNT_ASTERISK, AVG, SUM, MIN, MAX
	SqlValueType		param_type;		/* Data type (STRING_LITERAL, NUMERIC_LITERAL etc.) of function parameter.
							 * Initialized/Needed only if `type` is MIN_AGGREGATE or MAX_AGGREGATE.
							 */
	// SqlColumnList
	struct SqlStatement	*parameter;
} SqlAggregateFunction;

typedef struct SqlFunctionCall {
	// SqlValue
	struct SqlStatement	*function_name;
	// SqlFunction
	struct SqlStatement	*function_schema;
	// SqlColumnList
	struct SqlStatement	*parameters;
} SqlFunctionCall;

/**
 * Represents a SQL function
 */
typedef struct SqlFunction {
	struct SqlStatement	*function_name;			// SqlValue
	struct SqlStatement	*parameter_type_list;		// SqlParameterTypeList
	struct SqlStatement	*return_type;			// SqlDataType
	struct SqlStatement	*extrinsic_function;		// SqlValue
	int32_t			num_args;
	uint64_t		oid;
} SqlFunction;

typedef struct SqlDropFunctionStatement {
	struct SqlStatement	*function_name;			// SqlValue
} SqlDropFunctionStatement;

typedef struct SqlParameterTypeList {
	struct SqlStatement	*data_type;
	dqcreate(SqlParameterTypeList);
} SqlParameterTypeList;

typedef struct SqlValue {
	enum SqlValueType	type;
	enum SqlValueType	coerced_type;		/* initialized/usable only if `type` is COERCE_TYPE */
	enum SqlValueType	pre_coerced_type;	/* initialized/usable only if `type` is COERCE_TYPE */
	char			*parameter_index;
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
	// SqlValue, SqlColumnAlias, or SqlUnaryOperation
	struct SqlStatement *value;
	dqcreate(SqlColumnList);
} SqlColumnList;

// Structure to hold a table unique_id and a column number. Used in SqlColumnListAlias for ORDER BY columns
// that match an alias name of a column in the SELECT column list.
typedef struct SqlTableIdColumnId {
	int	unique_id;
	int	column_number;
} SqlTableIdColumnId;

typedef struct SqlColumnListAlias {
	// SqlColumnList
	struct SqlStatement		*column_list;
	// SqlValue
	struct SqlStatement		*alias;
	// Keywords used for the SORT column
	struct SqlStatement		*keywords;
	SqlValueType			type;
	boolean_t			user_specified_alias;
	/* The below field is needed to store the unique_id of the table and column number of the column in the
	 * SELECT column list of that table if this cla was matched to an ALIAS NAME from another cla
	 * (see "QUALIFY_COLUMN_REFERENCE" in qualify_statement.c).
	 */
	SqlTableIdColumnId		tbl_and_col_id;
	struct SqlColumnListAlias	*duplicate_of_column;	/* NULL mostly. If non-NULL (possible only in case of a
								 * NATURAL JOIN), this points to the column from a preceding
								 * table in the join list with the same name as this column.
								 */
	SqlColumnAlias			*outer_query_column_alias;	// the ColumnAlias structure corresponding to this
									// ColumnListAlias if/when referenced in outer query
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
	SqlSetOperationType	type;
	struct SqlStatement	*operand[2];
	SqlColumnListAlias	*col_type_list;	/* List of available columns with type information indicating the union of
						 * the types of the two operands of the SET operation. For example if this is
						 * an INTERSECT SET operation and the left operand has a column of type
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
} SqlCaseStatement;

typedef struct SqlCaseBranchStatement {
	// SqlValue
	struct SqlStatement *condition;
	// SqlValue
	struct SqlStatement *value;
	dqcreate(SqlCaseBranchStatement);
} SqlCaseBranchStatement;

typedef struct SqlSetStatement {
	struct SqlStatement	*variable;
	struct SqlStatement	*value;
} SqlSetStatement;

typedef struct SqlShowStatement {
	struct SqlStatement *variable;
} SqlShowStatement;

typedef struct SqlNoDataStatement {
	// Filler so compiler doesn't complain about empty type;
	// when we add something to this struct, simply replace this filler
	char b;
} SqlNoDataStatement;

typedef struct SqlStatement{
	enum SqlStatementType	type;
	struct YYLTYPE loc;
	union {
		struct SqlBeginStatement *begin;
		struct SqlCommitStatement *commit;
		struct SqlSelectStatement *select;
		struct SqlInsertStatement *insert;
		struct SqlDropTableStatement *drop_table;
		struct SqlValue *value;
		struct SqlFunctionCall *function_call;
		struct SqlAggregateFunction *aggregate_function;
		struct SqlBinaryOperation *binary;
		struct SqlUnaryOperation *unary;
		struct SqlColumnList *column_list;
		struct SqlColumn *column; // Note singular versus plural
		struct SqlJoin *join;
		struct SqlTable *create_table;
		struct SqlFunction *create_function;
		struct SqlDropFunctionStatement *drop_function;
		struct SqlParameterTypeList *parameter_type_list;
		struct SqlIndex *index;
		struct SqlOptionalKeyword *constraint;
		struct SqlOptionalKeyword *keyword;
		struct SqlColumnListAlias *column_list_alias;
		struct SqlColumnAlias *column_alias;
		struct SqlTableAlias *table_alias;
		struct SqlSetOperation *set_operation;
		struct SqlCaseStatement *cas;
		struct SqlCaseBranchStatement *cas_branch;
		struct SqlSetStatement *set;
		struct SqlShowStatement *show;
		struct SqlNoDataStatement *no_data;
		struct SqlDelimiterCharacterList *delim_char_list;
		enum SqlDataType data_type;
		enum SqlJoinType join_type;
	} v;
	uint64_t		hash_canonical_query_cycle;	// used during "hash_canonical_query" to avoid
								// multiple traversals of same node.
} SqlStatement;

#endif
