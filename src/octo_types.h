/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef OCTO_TYPES_H
#define OCTO_TYPES_H

typedef void *yyscan_t;

#include "memory_chunk.h"
#include "double_list.h"

#define INIT_YDB_BUFFER(buffer, len) (buffer)->buf_addr = malloc(len); (buffer)->len_used = 0; (buffer)->len_alloc = len;

#define SQL_STATEMENT(VAR, TYPE)			      \
	(VAR) = (SqlStatement*)octo_cmalloc(memory_chunks, sizeof(SqlStatement));  \
	(VAR)->type = TYPE;

#define MALLOC_STATEMENT(VAR, NAME, TYPE)	      \
	(VAR)->v.NAME = octo_cmalloc(memory_chunks, sizeof(TYPE));

#define UNPACK_SQL_STATEMENT(result, item, StatementType) assert((item)->type == StatementType ## _STATEMENT); \
	(result) = (item)->v.StatementType

#define PACK_SQL_STATEMENT(out, item, StatementType) \
	SQL_STATEMENT(out, StatementType ## _STATEMENT); \
	(out)->v.StatementType = item;

#define INIT_YDB_BUFFER(buffer, len) (buffer)->buf_addr = malloc(len); (buffer)->len_used = 0; (buffer)->len_alloc = len;

#define SAFE_SNPRINTF(buff_ptr, buffer, buffer_size, ...) \
	(buff_ptr) += snprintf((buff_ptr), (buffer_size) - ((buff_ptr) - (buffer)), ## __VA_ARGS__); \
	assert(buff_ptr - buffer > 0);


long long unsigned int typedef uint8;

enum SqlStatementType {
	table_STATEMENT,
	select_STATEMENT,
	insert_STATEMENT,
	drop_STATEMENT,
	value_STATEMENT,
	function_call_STATEMENT,
	binary_STATEMENT,
	unary_STATEMENT,
	column_list_STATEMENT,
	column_STATEMENT,
	join_STATEMENT,
	data_type_STATEMENT,
	constraint_STATEMENT,
	constraint_type_STATEMENT,
	keyword_STATEMENT,
	column_list_alias_STATEMENT,
	column_alias_STATEMENT,
	table_alias_STATEMENT,
	join_type_STATEMENT,
	set_operation_STATEMENT,
	begin_STATEMENT,
	commit_STATEMENT
};

enum UnaryOperations {
	FORCE_NUM,
	NEGATIVE,
	BOOLEAN_NOT
};

// The order of these must be mainted with LPActionType
enum BinaryOperations {
	ADDITION,
	SUBTRACTION,
	DVISION,
	MULTIPLICATION,
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
	BOOLEAN_IN,
	BOOLEAN_NOT_IN,
	BOOLEAN_NULL,
	BOOLEAN_NOT_NULL
};

enum SqlValueType {
	UNKNOWN_SqlValueType,
	NUMBER_LITERAL,
	STRING_LITERAL,
	DATE_TIME,
	COLUMN_REFERENCE,
	CALCULATED_VALUE,
	TEMPORARY_TABLE_TYPE,
	FUNCTION_NAME,
	BOOLEAN_VALUE,
	PARAMETER_VALUE,
	NUL_VALUE
} typedef SqlValueType;

enum SqlDataType {
	UNKNOWN_SqlDataType,
	CHARACTER_STRING_TYPE,
	INTEGER_TYPE,
	DATE_TIME_TYPE,
	INTERVAL_TYPE
};

enum OptionalKeyword {
	NO_KEYWORD,
	OPTIONAL_SOURCE,
	OPTIONAL_CURSE,
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
	OPTIONAL_POPULATE_INDEX, // not sure if this should be here; gets populated through LP
	UNKNOWN_SqlConstraintType,
	NOT_NULL,
	UNIQUE_CONSTRAINT,
	PRIMARY_KEY,
	REFERENCES,
	CHECK_CONSTRAINT,
	MAX_LENGTH,
};

enum SqlSetOperationType {
	SET_UNION,
	SET_UNION_ALL,
	SET_EXCEPT,
	SET_EXCEPT_ALL,
	SET_INTERSECT,
	SET_INTERSECT_ALL
} typedef SqlSetOperationType;

enum SqlJoinType {
	NO_JOIN,
	TABLE_SPEC,
	CROSS_JOIN,
	INNER_JOIN,
	RIGHT_JOIN,
	LEFT_JOIN,
	FULL_JOIN,
	NATURAL_JOIN
};

#define YYLTYPE yyltype

typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
	int first_line;
	int first_column;
	int last_line;
	int last_column;
};

struct SqlColumn typedef SqlColumn;
struct SqlColumnAlias typedef SqlColumnAlias;
//struct SqlConstraint;
struct SqlSelectStatement typedef SqlSelectStatement;
struct SqlInsertStatement typedef SqlInsertStatement;
struct SqlDropStatement  typedef SqlDropStatement;
struct SqlUnaryOperation typedef SqlUnaryOperation;
struct SqlBinaryOperation typedef SqlBinaryOperation;
struct SqlFunctionCall typedef SqlFunctionCall;
struct SqlValue typedef SqlValue;
struct SqlColumnList typedef SqlColumnList;
struct SqlTable typedef SqlTable;
struct SqlTableAlias typedef SqlTableAlias;
struct SqlJoin typedef SqlJoin;
struct SqlColumnListAlias typedef SqlColumnListAlias;
struct SqlStatement typedef SqlStatement;
struct SqlSetOperation typedef SqlSetOperation;
struct SqlBeginStatement typedef SqlBeginStatement;
struct SqlCommitStatement typedef SqlCommitStatement;

/**
 * Represents a SQL column; doubly linked list
 *
 * WARNING: in some cases, SqlColumnList is used instead of the linked list, namely when
 *  we are dealing with a SELECT column list because the column may be a calculated column
 */
struct SqlColumn
{
	SqlStatement *columnName;
	enum SqlDataType type;
	SqlStatement *table;
	SqlStatement *keywords;
	dqcreate(SqlColumn);
};

struct SqlColumnAlias
{
	// SqlColumn or SqlColumnListAlias
	SqlStatement *column;
	// SqlTableAlias
	SqlStatement *table_alias;
};

/**
 * Represents a SQL table
 */
struct SqlTable
{
	SqlStatement *tableName;
	SqlStatement *source;
	SqlStatement *columns;
	SqlStatement *delim;
	dqcreate(SqlTable);
};

struct SqlTableAlias
{
	// SqlTable or SqlSelectStatement
	SqlStatement *table;
	// SqlValue
	SqlStatement *alias;
	int unique_id;
	// SqlColumnListAlias list of available columns
	SqlStatement *column_list;
};

/**
 * Represents an optional KEYWORD which has a value associated with it */
struct SqlOptionalKeyword
{
	enum OptionalKeyword keyword;
	// Keyword value (SqlValue) or UNION statement (SqlSelectStatement)
	SqlStatement *v;
	dqcreate(SqlOptionalKeyword);
} typedef SqlOptionalKeyword;

/**
 * Effectively provides a list of tables that may or may not be joined
 */
struct SqlJoin
{
	// SqlTableAlias
	//  -> was SqlTable, should be changed everywhere
	SqlStatement *value;
	// SqlValue
	SqlStatement *condition;
	dqcreate(SqlJoin);
	enum SqlJoinType type;
};

/**
 * Represents a SQL SELECT statement
 */
struct SqlSelectStatement
{
	// SqlColumnListAlias
	SqlStatement *select_list;
	// SqlJoin
	SqlStatement *table_list;
	// SqlValue (?)
	SqlStatement *where_expression;
	// SqlValue (?)
	SqlStatement *order_expression;
	// SqlOptionalKeyword
	SqlStatement *optional_words;
	// SqlSetOperation
	SqlStatement *set_operation;
};

struct SqlInsertStatement
{
	SqlTable *destination;
	SqlStatement *source;
	SqlStatement *columns;
};

struct SqlDropStatement
{
	SqlStatement *table_name, *optional_keyword;
};


/*
 * Represents an binary operation
 */
struct SqlUnaryOperation
{
	enum UnaryOperations operation; // '+', '-'
	struct SqlStatement *operand;
};

/*
 * Represents an arithmetic operation
 */
struct SqlBinaryOperation
{
	enum BinaryOperations operation; // '+', '-', '*', '/'
	struct SqlStatement *operands[2];
};

struct SqlFunctionCall {
	// SqlValue
	SqlStatement *function_name;
	// SqlColumnList
	SqlStatement *parameters;
};

struct SqlValue {
	enum SqlValueType type;
	enum SqlDataType data_type;
	union {
		char *string_literal;
		char *reference;
		// SqlBinaryOperation, SqlUnaryOperation, SqlFunctionCall
		SqlStatement *calculated;
	} v;
};

/**
 * Used to represent a SELECT column list, not a table column list
 */
struct SqlColumnList {
	// SqlValue or SqlColumnAlias
	SqlStatement *value;
	dqcreate(SqlColumnList);
};

struct SqlColumnListAlias {
	// SqlColumnList
	SqlStatement *column_list;
	// SqlValue
	SqlStatement *alias;
	// Keywords used for the SORT column
	SqlStatement *keywords;
	SqlValueType type;
	dqcreate(SqlColumnListAlias);
};

/*
 * A SQL set operation, such as UNION, EXCEPT, or INTERSECT
 *
 * This is seperated from a binary operation because, at this time, I believe it
 *  to be not useable as part of an expression; if this proves to be wrong,
 *  we should consider merging it
 */
struct SqlSetOperation {
	SqlSetOperationType type;
	SqlStatement *operand[2];
};

struct SqlBeginStatement {
};

struct SqlCommitStatement {
};

struct SqlStatement {
	enum SqlStatementType type;
	struct YYLTYPE loc;
	union {
		SqlBeginStatement *begin;
		SqlCommitStatement *commit;
		SqlSelectStatement *select;
		SqlInsertStatement *insert;
		SqlDropStatement *drop;
		SqlValue *value;
		SqlFunctionCall *function_call;
		SqlBinaryOperation *binary;
		SqlUnaryOperation *unary;
		SqlColumnList *column_list;
		SqlColumn *column; // Note singular versus plural
		SqlJoin *join;
		SqlTable *table;
		SqlOptionalKeyword *constraint;
		SqlOptionalKeyword *keyword;
		SqlColumnListAlias *column_list_alias;
		SqlColumnAlias *column_alias;
		SqlTableAlias *table_alias;
		SqlSetOperation *set_operation;
		enum SqlDataType data_type;
		enum SqlJoinType join_type;
	} v;
};

/// TODO: this should be located in octo.h, but for some reason it's not being detected there
SqlStatement *natural_join_condition(SqlStatement *left, SqlStatement *right);
#endif
