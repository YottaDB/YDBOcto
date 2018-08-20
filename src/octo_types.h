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

enum SqlValueType {
  UNKNOWN_SqlValueType,
  NUMBER_LITERAL,
  STRING_LITERAL,
  DATE_TIME,
  COLUMN_REFERENCE,
  CREATE_TABLE_STATEMENT
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

struct SqlColumn;
struct SqlConstraint;
struct SqlCreateTableStatement;

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
  dqcreate(SqlColumn);
} typedef SqlColumn;

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

struct SqlValue {
  enum SqlValueType type;
  union {
    char *string_literal;
    char *column_reference;
  } value;
} SqlVale;

#endif
