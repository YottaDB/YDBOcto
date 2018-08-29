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

#define MAX_STR_CONST 1024
#define MAX_EXPRESSION_LENGTH 255
typedef void *yyscan_t;

// Sets a DQ struct to point to itself
#define dqinit(object) (object)->next = object, (object)->prev = object
// Defines the elements for a DQ struct
#define dqcreate(struct_type) struct struct_type *next, *prev
// Inserts an element behind this one in the doubly linked list
#define dqinsert(self, new_elem) (new_elem)->prev = (self)->prev, \
  (self)->prev->next = (new_elem), (self)->prev = new_elem, (new_elem)->next = self;

#define SQL_STATEMENT(VAR, TYPE) (VAR) = (SqlStatement*)malloc(sizeof(SqlStatement)); (VAR)->type = TYPE;
#define UNPACK_SQL_STATEMENT(result, item, StatementType) assert((item)->type == StatementType##_STATEMENT); \
  (result) = (item)->v.StatementType

long long unsigned int typedef uint8;

enum SqlStatementType {
  table_STATEMENT,
  select_STATEMENT,
  drop_STATEMENT,
  value_STATEMENT,
  binary_STATEMENT,
  unary_STATEMENT,
  column_list_STATEMENT,
  column_STATEMENT,
  join_STATEMENT,
  data_type_STATEMENT,
  constraint_STATEMENT,
  constraint_type_STATEMENT,
  keyword_STATEMENT
};

enum UnaryOperations {
  FORCE_NUM,
  NEGATIVE
};

enum BinaryOperations {
  ADDITION,
  SUBTRACTION,
  DVISION,
  MULTIPLICATION,
  CONCAT
};

enum SqlValueType {
  UNKNOWN_SqlValueType,
  NUMBER_LITERAL,
  STRING_LITERAL,
  DATE_TIME,
  COLUMN_REFERENCE,
  CALCULATED_VALUE
};

enum SqlDataType {
  UNKNOWN_SqlDataType,
  CHARACTER_STRING_TYPE,
  INTEGER_TYPE,
  DATE_TIME_TYPE,
  INTERVAL_TYPE
};

enum SqlConstraintType {
  UNKNOWN_SqlConstraintType,
  NOT_NULL,
  UNIQUE_CONSTRAINT,
  PRIMARY_KEY,
  REFERENCES,
  CHECK_CONSTRAINT,
  MAX_LENGTH
};

enum OptionalKeyword {
  NO_KEYWORD,
  OPTIONAL_SOURCE,
  OPTIONAL_CURSE,
  OPTIONAL_END,
  OPTIONAL_START,
  OPTIONAL_CASCADE,
  OPTIONAL_RESTRICT
};

enum SqlJoinType {
  NO_JOIN
};

struct SqlColumn;
struct SqlConstraint;
struct SqlSelectStatement;
struct SqlUnaryOperation;
struct SqlBinaryOperation;
struct SqlValue typedef SqlValue;
struct SqlColumnList typedef SqlColumnList;
struct SqlTable typedef SqlTable;
struct SqlJoin typedef SqlJoin;
struct SqlStatement typedef SqlStatement;

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
  SqlStatement *constraints;
  SqlStatement *tableName; // If not null, qualified name was used
  dqcreate(SqlColumn);
} typedef SqlColumn;

struct SqlColumnAlias
{
  SqlStatement *column;
  SqlStatement *alias;
} typedef SqlColumnAlias;

/**
 * Represents a SQL constraint type; doubly linked list
 */
struct SqlConstraint
{
  enum SqlConstraintType type;
  SqlStatement *referencesColumn; //snprintf(buffer, 255 in the form of table.column
  SqlStatement *check_constraint_definition; // as a piece of MUMPS code
  uint8 max_length;
  dqcreate(SqlConstraint);
} typedef SqlConstraint;

/**
 * Represents a SQL table
 */
struct SqlTable
{
  SqlStatement *tableName;
  SqlStatement *source;
  SqlStatement *columns;
  SqlStatement *curse;
  SqlStatement *start;
  SqlStatement *end;
  dqcreate(SqlTable);
};

/**
 * Represents an optional KEYWORD which has a value associated with it */
struct SqlOptionalKeyword
{
  enum OptionalKeyword keyword;
  SqlStatement *v;
  dqcreate(SqlOptionalKeyword);
} typedef SqlOptionalKeyword;

/**
 * Effectively provides a list of tables that may or may not be joined
 */
struct SqlJoin
{
  SqlStatement *value;
  SqlStatement *next;
  enum SqlJoinType type;
};

/**
 * Represents a SQL SELECT statement
 */
struct SqlSelectStatement
{
  SqlStatement *select_list;
  SqlStatement *table_list;
} typedef SqlSelectStatement;

/*
 * Represents an binary operation
 */
struct SqlUnaryOperation
{
  enum UnaryOperations operation; // '+', '-'
  struct SqlStatement *operand;
} typedef SqlUnaryOperation;

/*
 * Represents an arithmetic operation
 */
struct SqlBinaryOperation
{
  enum BinaryOperations operation; // '+', '-', '*', '/'
  struct SqlStatement *operands[2];
} typedef SqlBinaryOperation;

struct SqlValue {
  enum SqlValueType type;
  union {
    char *string_literal;
    char *reference;
    SqlStatement *calculated;
  } v;
};

struct SqlDropStatement {
  SqlStatement *table_name, *optional_keyword;
} typedef SqlDropStatement;

struct SqlColumnList {
  SqlStatement *value;
  SqlStatement *next;
};

struct SqlStatement {
  enum SqlStatementType type;
  union {
    SqlSelectStatement *select;
    SqlDropStatement *drop;
    SqlValue *value;
    SqlBinaryOperation *binary;
    SqlUnaryOperation *unary;
    SqlColumnList *column_list;
    SqlColumn *column; // Note singular versus plural
    SqlJoin *join;
    SqlTable *table;
    SqlConstraint *constraint;
    SqlOptionalKeyword *keyword;
    enum SqlDataType data_type;
    enum SqlConstraintType constraint_type;
  } v;
};

SqlTable *definedTables;

#endif
