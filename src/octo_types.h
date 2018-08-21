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
typedef void *yyscan_t;

// Sets a DQ struct to point to itself
#define dqinit(object) (object)->next = object, (object)->prev = object
// Defines the elements for a DQ struct
#define dqcreate(struct_type) struct struct_type *next, *prev
// Inserts an element behind this one in the doubly linked list
#define dqinsert(self, new_elem) (new_elem)->prev = (self)->prev (self)->prev = new_elem, (new_elem)->next = self;

enum SqlStatementType {
  CREATE_TABLE_STATEMENT,
  SELECT_STATEMENT,
  SQL_VALUE,
  BINARY_OPERATION,
  UNARY_OPERATION,
  COLUMN_LIST,
  JOIN_STATEMENT,
  TABLE_STATEMENT
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
  CHECK_CONSTRAINT
};

enum SqlJoinType {
  NO_JOIN
};

struct SqlColumn;
struct SqlConstraint;
struct SqlCreateTableStatement;
struct SqlSelectStatement;
struct SqlUnaryOperation;
struct SqlBinaryOperation;
struct SqlValue typedef SqlValue;
struct SqlColumnList typedef SqlColumnList;
struct SqlTable;
struct SqlJoin typedef SqlJoin;
struct SqlStatement typedef SqlStatement;

struct SqlCreateTableStatement
{
  char *tableName;
  struct SqlColumn *columns;
} typedef SqlCreateTableStatement;

/**
 * Represents a SQL column; doubly linked list
 */
struct SqlColumn
{
  char *columnName;
  enum SqlDataType type;
  struct SqlConstraint *constraints;
  char *tableName; // If not null, qualified name was used
  dqcreate(SqlColumn);
} typedef SqlColumn;

struct SqlColumnAlias
{
  SqlColumn *column;
  char *alias;
} typedef SqlColumnAlias;

/**
 * Represents a SQL constraint type; doubly linked list
 */
struct SqlConstraint
{
  enum SqlConstraintType type;
  char *referencesColumn; // in the form of table.column
  char *check_constraint_definition; // as a piece of MUMPS code
  dqcreate(SqlConstraint);
} typedef SqlConstraint;

/**
 * Represents a SQL table
 */
struct SqlTable
{
  char *tableName;
  struct SqlColumn *columns;
} typedef SqlTable;

/**
 * Effectively provides a list of tables that may or may not be joined
 */
struct SqlJoin
{
  SqlTable *value;
  SqlJoin *next;
  enum SqlJoinType type;
};

/**
 * Represents a SQL SELECT statement
 */
struct SqlSelectStatement
{
  SqlColumnList *select_list;
  SqlJoin *table_list;
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
    char *column_reference;
    SqlStatement *calculated;
  } v;
};

struct SqlColumnList {
  SqlStatement *value;
  SqlColumnList *next;
};

struct SqlStatement {
  enum SqlStatementType type;
  union {
    SqlSelectStatement *select;
    SqlValue *value;
    SqlBinaryOperation *binary;
    SqlUnaryOperation *unary;
    SqlColumnList *columns;
    SqlJoin *join;
    SqlTable *table;
  } v;
};

#endif
