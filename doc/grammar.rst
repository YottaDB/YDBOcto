
================
DBMS Grammar
================

.. contents::
   :depth: 4

A SQL statement can be a Schema statement, a Data statement, or a SELECT statement.

A Schema statement creates and manipulates a unique schema within the database.

A Data statement is any statement that makes a change to the data in the database. Changes to data can be brought about by deleting data, inserting new data or updating existing data.

A SELECT statement is used to select and view data from the database.

.. note::
   Some functions and keywords described in this documentation have yet to be implemented, and currently generate an error as we move toward a complete implementation.

.. note::
   Comments can be placed within SQL statements using :code:`--`, :code:`#` or the :code:`/*...*/` symbols.

---------------------
Accepted Data Types
---------------------

+++++++++++++++++++++
Character Data Types
+++++++++++++++++++++

* CHARACTER
* CHAR
* CHARACTER VARYING
* CHAR VARYING
* VARCHAR

Octo does not differentiate between these data types. They can be used to represent character values, and can optionally be followed by a precision value in parentheses. Example: char(20).

++++++++++++++++++++
Numeric Data Types
++++++++++++++++++++

* NUMERIC
* DECIMAL
* DEC
* INTEGER
* INT
* INT2
* INT4
* INT8
* SMALLINT
* BIGINT

Note that Octo does not differentiate between the various integer types listed above, internally treating them all as a single integer type. Similarly, Octo treats NUMERIC and various decimal types interchangably under a single numeric type.

Details about the range and accuracy of both DECIMAL/NUMERIC and INTEGER types can be found in the YottaDB `M Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/langfeat.html#numeric-accuracy>`_.

NUMERIC, DECIMAL and DEC can optionally be followed by a precision value in parentheses. Example: dec(10).

.. note::
   The specified precision values are ignored when queries are executed.

++++++++++++++++++++
Boolean Data Type
++++++++++++++++++++

Octo uses :code:`0` and :code:`1` internally to represent boolean :code:`false` and :code:`true` respectively. However :code:`true` and :code:`false` can be used in Octo queries in the following manner:

.. code-block:: SQL

   SELECT * FROM names WHERE true;

.. note::

   Octo doesn't support :code:`t/f` like PostgreSQL does.


---------------
CREATE TABLE
---------------

.. code-block:: SQL

   CREATE TABLE table_name (column_name data_type [constraints][, ... column_name data_type [constraints]]) [optional_keyword];

The CREATE TABLE statement is used to create tables in the database. The keywords CREATE TABLE are used followed by the name of the table to be created.

The names of columns to be created in the database and their datatypes are then specified in a list, along with any constraints that might need to apply (such as denoting a PRIMARY KEY, UNIQUE KEY or FOREIGN KEY). If none of the columns are specified as keys (PRIMARY KEY or KEY NUM not specified in any column) then the primary key for the table is assumed to be the set of all columns in the order given.

Example:

.. code-block:: SQL

   CREATE TABLE Employee (ID int PRIMARY KEY, FirstName char(20), LastName char(30));

   CREATE TABLE Employee (ID int, FirstName char(20), LastName char(30));
   /* is equivalent to */
   CREATE TABLE (ID int KEY NUM 0, FirstName char(20) KEY NUM 1, LastName char(30) KEY NUM 2);

Note that CREATE TABLE statements can also accept a list of ASCII integer values for use in the DELIM qualifier, for example:

.. code-block:: SQL

   CREATE TABLE DELIMNAMES (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), middleInitial VARCHAR(1), age INTEGER) DELIM (9, 9) GLOBAL "^delimnames(keys(""id""))";

Here, two TAB characters (ASCII value 9) act as the internal delimiter of an Octo table. Note, however, that these delimiters are not applied to Octo output, which retains the default pipe :code:`|` delimiter. The reason for this is that tables may be joined that have different delimiters, so one common delimiter needs to be chosen anyway. Thus, the default is used.

Similarly, CREATE TABLE statements can also accept an ASCII integer value to specify a character to interpret as a SQL NULL value:

.. code-block:: SQL

   CREATE TABLE nullcharnames (id INTEGER PRIMARY KEY, firstName VARCHAR(30) NOT NULL, lastName TEXT(30)) NULLCHAR (127) GLOBAL "^nullcharnames(keys(""id""))";

Here, the ASCII value for DEL[ETE] is designated to be interpreted as a SQL NULL value.

+++++++++++++++++++++++++++++++++++++++++++++
Mapping to existing YottaDB global variables
+++++++++++++++++++++++++++++++++++++++++++++

If mapping to existing YottaDB global variables, an optional_keyword can be added to further enhance the CREATE statement:

.. code-block:: none

   [ADVANCE | CURSOR | DELIM | END | EXTRACT | GLOBAL | KEY NUM | NULLCHAR | PIECE LITERAL]

The keywords denoted above are M expressions and literals. They are explained in the following table:

+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| Keyword                        | Type                          | Range                  | Purpose                                                                        | Overrides                    | Default Value                |
+================================+===============================+========================+================================================================================+==============================+==============================+
| ADVANCE                        | Command expression            | Column                 | Indicates how to advance the key by one value                                  | \-                           | $O(^<tableName>(keys(0),...))|
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| CURSOR                         | Command expression            | Table                  | Increment the cursor by one element                                            | \-                           | SET keys(0)=$0(table_name(   |
|                                |                               |                        |                                                                                |                              | keys(0)))                    |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| DELIM                          | Literal                       | Table, Column          | Represents the "PIECE" string to be used in                                    | table/default DELIM setting  | \|                           |
|                                |                               |                        | `$PIECE <https://docs.yottadb.com/ProgrammersGuide/functions.html#piece>`_     |                              |                              |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| END                            | Boolean expression            | Table                  | Indicates that the cursor has hit the last record in the table                 | \-                           | \"\"=keys(0)                 |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| EXTRACT                        | Expression                    | Column                 | Extracts the value of the column from the database                             | PIECE, GLOBAL                | \-                           |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| GLOBAL                         | Literal                       | Table, Column          | Represents the "source" location for a table                                   | table/default GLOBAL setting | table_name(keys(0))          |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| KEY NUM                        | Literal                       | Column                 | Specifies that the column maps to keys(<number>)                               | \-                           | \-                           |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| NULLCHAR                       | Literal                       | Table, Column          | Specifies a custom character to be interpreted as a SQL NULL value. Characters | default interpretation of    | \"\"                         |
|                                |                               |                        | are specified as an integer ASCII value from 0-127 to be used in a call to     | empty strings as NULL values |                              |
|                                |                               |                        | `$CHAR <https://docs.yottadb.com/ProgrammersGuide/functions.html#char>`_       |                              |                              |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| PIECE                          | Literal                       | Column                 | Represents the                                                                 | default (column number,      | \-                           |
|                                |                               |                        | `$PIECE <https://docs.yottadb.com/ProgrammersGuide/functions.html#piece>`_     | starting at 1)               |                              |
|                                |                               |                        | number of the row this column refers to                                        |                              |                              |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| START                          | Command expression            | Column                 | Indicates where to start a $ORDER loop in the underlying data storage          | \-                           | ""                           |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+

In the table above:

* table_name and cursor_name are variables representing the names of the table and the cursor being used.
* keys is a special variable in Octo that contains all of the columns that are identified as keys in the DDL (either via the "PRIMARY KEY" or "KEY NUM X" set of keywords).

+++++++++++++
Error Case
+++++++++++++

.. note::
   A CREATE TABLE waits for all other concurrently running queries(SELECT or CREATE TABLE or DROP TABLE) to finish so it can safely make DDL changes. It waits for an exclusive lock with a timeout of 10 seconds. If it fails due to a timeout, the user needs to stop all concurrently running queries and reattempt the CREATE TABLE statement.

---------------
CREATE FUNCTION
---------------

.. code-block:: SQL

   CREATE FUNCTION function_name([data_type[, data_type[, ...]]]) RETURNS data_type AS extrinsic_function_name;

The CREATE FUNCTION statement is used to create SQL functions that map to extrinsic M functions and store these mappings in the database. The keywords CREATE FUNCTION are followed by the name of the SQL function to be created, the data types of its parameters, its return type, and the fully-qualified extrinsic M function name.

CREATE FUNCTION can be used to define multiple functions with the same name, provided the number of parameters and/or the types of the parameters are different. In other words, CREATE FUNCTION supports function overloading.

However, functions cannot be overloaded based on their return type. For example, if two CREATE FUNCTION calls are made with the same name and parameter types, but a different return type, the return type of the last executed statement will be retained and the first discarded. Accordingly, care should be used when overloading functions, particularly when specifying varied return types for a single function.

The SQL function's parameter data types are specified in a list, while the data type of the return value must be a single value (only one object can be returned from a function). The extrinsic function name must be of the form detailed in the `M Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/langfeat.html#id8>`__.

When a function is created from a CREATE FUNCTION statement, an entry is added to Octo's internal PostgreSQL catalog. In other words, a row is added to the :code:`pg_catalog.pg_proc` system table. To view a list of created functions, their argument number and type(s), and return argument type, you can run:

.. code-block:: SQL

   select proname,pronargs,prorettype,proargtypes from pg_proc;

Type information for each function parameter and return type will be returned as an OID. This OID can be used to look up type information, including type name, from the :code:`pg_catalog.pg_type` system table. For example, to retrieve the human-readable return type and function name of all existing functions:

.. code-block:: SQL

   select proname,typname from pg_catalog.pg_proc inner join pg_catalog.pg_type on pg_catalog.pg_proc.prorettype = pg_catalog.pg_type.oid;

However, function parameter types are currently stored as a list in a VARCHAR string, rather than in a SQL array as the latter isn't yet supported by Octo. In the meantime, users can lookup the type name corresponding to a given type OID by using the following query:

.. code-block:: SQL

   select oid,typname from pg_catalog.pg_type;

Note that CREATE FUNCTION is the preferred method for creating new SQL functions and manually creating these functions through direct database modifications is not advised.

Example:

.. code-block:: none

   CREATE FUNCTION ADD(int, int) RETURNS int AS $$ADD^myextrinsicfunction;

   CREATE FUNCTION APPEND(varchar, varchar) RETURNS varchar AS $$APPEND;

To create a parameterless function, the parameter type list may be omitted by leaving the parentheses blank:

Example:

.. code-block:: none

   CREATE FUNCTION userfunc() RETURNS int AS $$userfunc^myextrinsicfunction;

+++++++++++++
Error Case
+++++++++++++

.. note::
   A CREATE FUNCTION waits for all other concurrently running queries(SELECT or CREATE TABLE or DROP TABLE) to finish so it can safely make DDL changes. It waits for an exclusive lock with a timeout of 10 seconds. If it fails due to a timeout, the user needs to stop all concurrently running queries and reattempt the CREATE FUNCTION statement.

-----------------
DROP TABLE
-----------------

.. code-block:: SQL

   DROP TABLE table_name [CASCADE | RESTRICT];

The DROP TABLE statement is used to remove tables from the database. The keywords DROP TABLE are followed by the name of the table desired to be dropped. Optional parameters include CASCADE and RESTRICT.

The CASCADE parameter is used to specify that all objects depending on the table will also be dropped.

The RESTRICT parameter is used to specify that the table referred to by table_name will not be dropped if there are existing objects depending on it.

Example:

.. code-block:: SQL

   DROP TABLE Employee CASCADE;

+++++++++++++
Error Case
+++++++++++++

.. note::
   A DROP TABLE waits for all other concurrently running queries(SELECT or CREATE TABLE or DROP TABLE) to finish so it can safely make DDL changes. It waits for an exclusive lock with a timeout of 10 seconds. If it fails due to a timeout, the user needs to stop all concurrently running queries and reattempt the DROP TABLE statement.

-----------------
DROP FUNCTION
-----------------

.. code-block:: SQL

   DROP FUNCTION function_name [(arg_type [, ...])];

The DROP FUNCTION statement is used to remove functions from the database. The keywords DROP FUNCTION are followed by the name of the function desired to be dropped and a list of the parameter types expected by the function. These types, if any, must be included as multiple functions may exist with the same name, but must have different parameter type lists.

Note also that the function name provided should be the name of the user-defined SQL function name, not the M label or routine name.

A function deleted using the DROP FUNCTION statement will also be removed from Octo's internal PostgreSQL catalog. In other words, the function will be removed from the :code:`pg_catalog.pg_proc` system table.

The following example demonstrates two ways of dropping a function that has no parameters:

.. code-block:: SQL

   DROP FUNCTION userfunc;
   DROP FUNCTION userfunc();

This example demonstrates dropping a function with parameters of types VARCHAR and INTEGER:

.. code-block:: SQL

   DROP FUNCTION userfuncwithargs (VARCHAR, INTEGER);

+++++++++++++
Error Case
+++++++++++++

.. note::
   A DROP FUNCTION waits for all other concurrently running queries(SELECT or CREATE TABLE or DROP TABLE) to finish so it can safely make DDL changes. It waits for an exclusive lock with a timeout of 10 seconds. If it fails due to a timeout, the user needs to stop all concurrently running queries and reattempt the DROP FUNCTION statement.

-----------
SELECT
-----------

The SELECT statement is used to select rows from the database by specifying a query, and optionally sorting the resulting rows.

.. code-block:: PSQL

   SELECT [ALL | DISTINCT]
   [ * | expression [[AS] alias_name] [, ...]]
   [FROM from_item [, ...]]
   [WHERE search_condition]
   [GROUP BY grouping_column [, ...]]
   [HAVING search_condition]
   [{UNION | INTERSECT | EXCEPT} select]
   [ORDER BY sort_specification]
   [LIMIT number];

+++++
ALL
+++++

The use of this clause returns all rows, which is the default behavior.

++++++++++
DISTINCT
++++++++++

The use of this clause returns only non-duplicate rows (keeping one each from the set of duplicates).

++++++
FROM
++++++

This clause specifies the table(s) from which the columns are selected.

**from_item** can be any of the following:

    - A table name
    - An alias
    - A SELECT subquery, which must be surrounded by parentheses.
    - A join

~~~~~~~
JOINS
~~~~~~~

Joins can be made by appending a join type and table name to a SELECT statement:

.. code-block:: SQL

   [CROSS | [NATURAL | INNER | [LEFT][RIGHT][FULL] OUTER]] JOIN ON joined_table;

A **CROSS JOIN** between two tables provides the number of rows in the first table multiplied by the number of rows in the second table.

A **QUALIFIED JOIN** is a join between two tables that specifies a join condition.

A **NATURAL JOIN** is a join operation that combines tables based on columns with the same name and type. The resultant table does not contain repeated columns.

**Types of Joins**:

For two tables, Table A and Table B,

- **Inner Join** : Only the common rows between Table A and Table B are returned.
- **Outer Join**

  - **Left Outer Join** : All rows from Table A are returned, along with matching rows from Table B.
  - **Right Outer Join** : Matching rows from Table A are returned, along with all rows from Table B.
  - **Full Outer Join** : All matching rows from Table A and Table B are returned, followed by rows from Table A that have no match and rows from Table B that have no match.

Example:

.. code-block:: SQL

   SELECT FirstName, LastName, Address
   FROM Employee INNER JOIN Addresses
   ON Employee.ID = Addresses.EID;

++++++++
WHERE
++++++++

This clause represents a condition under which columns are selected. If the **search_condition** evaluates to true, that row is part of the output otherwise it is excluded.

+++++++++++
GROUP BY
+++++++++++

The GROUP BY clause ensures that the resulting rows are grouped together based on the specified **grouping_column**.

++++++++++
HAVING
++++++++++

The HAVING clause works to filter the rows that result from the GROUP BY clause. The rows are filtered based on the boolean value returned by the **search_condition**.

See :ref:`Technical Notes <technical-notes>` for details on value expressions.

Example:

.. code-block:: SQL

   SELECT ID, FirstName, LastName FROM Employee WHERE ID > 100 GROUP BY LastName;

++++++++++
ORDER BY
++++++++++

ORDER BY lets you sort the order of the rows returned after the query.

To sort rows or columns in the database, you need to have one of the following **sort_specifications**.

.. code-block:: SQL

   sort_key [COLLATE collation_name] [ASC | DESC];

The **sort_key** is either a column reference or a literal.

The sort key can be followed by a collate clause, ordering specification or both.

.. note::
   A collation is a set of rules to compare characters in a character set.

The collate clause consists of the word COLLATE and the relevant collation name.

The ordering specification lets you further choose whether to order the returned columns in ascending (ASC) or descending (DESC) order.

Example:

.. code-block:: SQL

   SELECT ID, FirstName, LastName FROM Employee WHERE ID > 100 ORDER BY ID DESC;

+++++++
LIMIT
+++++++

This clause allows the user to specify the number of rows they want to retrieve from the results of the query.

Example:

.. code-block:: SQL

   SELECT * FROM Employee LIMIT 5;

The above example returns no more than 5 rows.

+++++++++++++++++++++
Queries without rows
+++++++++++++++++++++

SELECT can also be used to calculate values, without needing to select from a table.

Example:

.. code-block:: SQL

   SELECT (1 * 2) + 3;

.. note::

   WHERE is currently not supported for SELECT statements without a FROM clause.
   This is known issue tracked at `YDBOcto#500 <https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/500>`_.

--------------
INSERT
--------------

*(Currently not supported.)*

.. code-block:: SQL

   INSERT INTO table_name ( column name [, column name ...]) [ VALUES ... | (SELECT ...)];

The INSERT statement allows you to insert values into a table. These can either be provided values or values specified as a result of a SELECT statement.

Example:

.. code-block:: SQL

   INSERT INTO Employee (ID , FirstName, LastName) [220, "Jon", "Doe"];

--------------
UPDATE
--------------

*(Currently not supported.)*

.. code-block:: SQL

   UPDATE table_name SET object_column EQUALS update_source [WHERE search_condition];

The UPDATE statement begins with the keyword UPDATE. The table_name to be updated and the keyword SET is followed by a list of comma-separated statements that are used to update existing columns, where object_column is a particular column and update_source is set to either NULL or a specific value expression. The optional WHERE condition allows you to update columns based on a certain condition you specify.

Example:

.. code-block:: SQL

   UPDATE Employee SET FirstName = "John" WHERE ID = 220;

------------
DELETE
------------

*(Currently not supported.)*

.. code-block:: SQL

   DELETE FROM table_name [WHERE search_condition];

The DELETE statement consists of the keywords DELETE FROM followed by the name of the table and possibly a search condition.

The search condition eventually yields a boolean true or false value, and may contain further search modifications detailing where to apply the search_condition and how to compare the resulting values.

Example:

.. code-block:: SQL

   DELETE FROM Employee WHERE ID = 220;

-------------------
Set Operations
-------------------

These are operations that work on the results of two or more queries.

The conditions are:

- The data types in the results of each query need to be compatible.
- The order and number of the columns in each result set need to be the same.

+++++++++++++++++
UNION
+++++++++++++++++

.. code-block:: SQL

   SELECT [.....] FROM table_name[...]  UNION [ALL] SELECT [.....] FROM table_name2[...]....;

The UNION operation consists of two or more queries joined together with the word UNION.  It combines the results of two individual queries into a single set of results.

The keyword ALL ensures that duplicate rows of results are not removed during the UNION.

Example:

.. code-block:: SQL

   SELECT FirstName FROM Employee UNION SELECT FirstName FROM AddressBook;

++++++++++++++++
INTERSECT
++++++++++++++++

.. code-block:: SQL

   SELECT [.....] FROM table_name[......] INTERSECT [ALL] SELECT [.....] FROM table_name2[....]......;

The INTERSECT operation consists of two or more queries joined together with the word INTERSECT. It returns distinct non-duplicate results that are returned by both queries on either side of the operation.

The keyword ALL ensures that duplicate rows of results returned by both queries are not eliminated during the INTERSECT.

.. code-block:: SQL

   SELECT ID FROM Employee INTERSECT SELECT ID FROM AddressBook;

++++++++++++++
EXCEPT
++++++++++++++

.. code-block:: SQL

   SELECT [.....] FROM table_name[.....] EXCEPT [ALL] SELECT [.....] FROM table_name2[......].......;

The EXCEPT operation consists of two or more queries joined together with the word EXCEPT. It returns (non-duplicate) results from the query on the left side except those that are also part of the results from the query on the right side.

The keyword ALL affects the resulting rows such that duplicate results are allowed but rows in the first table are eliminated if there is a corresponding row in the second table.

.. code-block:: SQL

   SELECT LastName FROM Employee EXCEPT SELECT LastName FROM AddressBook;

--------------
CASE
--------------

Octo supports two different formats of the CASE statement.

.. code-block:: SQL

   CASE value_expression
   WHEN value_1 THEN result_1
   WHEN value_2 THEN result_2
   [WHEN ... ]
   [ELSE result_n]
   END

This form of the CASE statement evaluates the value_expression and sequentially compares that to each of the values following WHEN. Upon finding a match it returns the corresponding "result" following THEN. If no match is found then the "result" following ELSE is returned, or NULL is returned if ELSE has been omitted.

.. code-block:: SQL

   CASE WHEN condition_expression_1 THEN result_1
	WHEN condition_expression_2 THEN result_2
	[WHEN ... ]
	[ELSE result_n]
   END

The second form of the CASE statement sequentially tests each condition_expression. If a condition_expression evaluates to TRUE, the "result" following THEN is returned. If all conditions evaluate to FALSE the "result" following ELSE is returned, or NULL is returned if ELSE has been omitted.


-----------------
COALESCE
-----------------

.. code-block:: SQL

   SELECT COALESCE(value_expression [, value_expression...]) ...

The built-in COALESCE function returns the first of its arguments that is not NULL.
If all arguments are NULL, NULL is returned.
COALESCE must have at least one argument.

Note that unlike other RDBMSs, the values passed to COALESCE are not required to all have the same type.
For example, the following query is valid and returns the value 1:

.. code-block:: SQL

   SELECT COALESCE(1, 'a', 1.0);

-------
NULLIF
-------

.. code-block:: SQL

   SELECT NULLIF(value_expression, value_expression) ...

The built-in NULLIF function returns NULL if both arguments are equal, or the first argument otherwise.
The arguments must have the same type.

-------------------
GREATEST and LEAST
-------------------

.. code-block:: SQL

   SELECT GREATEST(value_expression [, value_expression...]) ...
   SELECT LEAST(value_expression [, value_expression...]) ...

The built-in GREATEST function returns the largest value from a list of expressions.
Similarly, LEAST returns the smallest value.
NULL values are ignored, unless all values are NULL, in which case the return value is NULL.
All arguments must have the same type.

----------
Functions
----------

Octo supports the following pre-defined functions.

----
ABS
----

.. code-block:: SQL

   SELECT ABS(NUMERIC) ...

ABS returns the absolute value of a number.

------
ROUND
------

.. code-block:: SQL

   SELECT ROUND(NUMERIC, INTEGER) ...

ROUND returns the first argument rounded to the precision specified by the second argument.
If the precision is greater than 0, the number will be rounded to that number of decimal places.
If the precision is 0, it will be rounded to the nearest integer.
If the precision is less than 0, all fractional digits will be truncated and the number will be rounded to `10^precision`.
The precision must be no less than -46.

------
TRUNC
------

.. code-block:: SQL

   SELECT ROUND(NUMERIC, INTEGER) ...

TRUNC returns the first argument truncated to the precision specified by the second argument.
If the precision is greater than 0, the number will be truncated to that number of decimal places.
If the precision is 0, this behaves the same as the mathematical `floor` function.
If the precision is less than 0, all fractional digits will be truncated and the number will be truncated to `10^precision`.
The precision must be no less than -43.

-----------------
Operators
-----------------

The comparative operators in Octo are:

* EQUALS =
* NOT EQUALS <>
* LESS THAN <
* GREATER THAN >
* LESS THAN OR EQUALS <=
* GREATER THAN OR EQUALS >=

The logical operators in Octo are:

* AND : The record will be displayed if all the conditions are TRUE
* OR  : The record will be displayed if any of the conditions is TRUE
* NOT : The record will be displayed if the condition(s) is NOT TRUE

Other operators in Octo:

* BETWEEN  : This operator selects values within a given range, begin and end values included.
* EXISTS   : The result is TRUE if the evaluated subquery returns at least one row. It is FALSE if the evaluated subquery returns no rows.
* ANY/SOME : The result is TRUE if any true result is obtained when the expression is evaluated and compared to each row of the subquery result. It is FALSE if no true result is found or if the subquery returns no rows.

------------------------
Alias
------------------------

Double quotes, single quotes and non quoted identifiers can be used to represent alias names.

++++++++++++++
Column Alias
++++++++++++++

.. code-block:: SQL

   column [AS] aliasname

Examples:

.. code-block:: SQL

   OCTO> select firstname as "quoted" from names limit 1;
   Zero

   OCTO> select firstname as 'quoted' from names limit 1;
   Zero

   OCTO> select firstname as ida from names limit 1;
   Zero

   OCTO> select ida from (select 8 as "ida") n1;
   8

   OCTO> select ida from (select 8 as 'ida') n1;
   8

   OCTO> select ida from (select 8 as ida) n1;
   8

   OCTO> select ida from (select 8 as ida) as n1;
   8

Column aliases are supported in short form i.e without AS keyword

.. code-block:: SQL

   OCTO> select ida from (select 8 ida) n1;
   8

+++++++++++++++
Table Alias
+++++++++++++++

Usage:

.. code-block:: SQL

   [table_name | subquery] [AS] aliasname

Examples:

.. code-block:: SQL

   OCTO> select n1.firstname from names as "n1" limit 1;
   Zero

   OCTO> select n1.firstname from names as 'n1' limit 1;
   Zero

   OCTO> select n1.firstname from names as n1 limit 1;
   Zero

   OCTO> select 1 from names as n1 inner join (select n2.id from names as n2 LIMIT 3) as alias2 ON (n1.id = alias2.id );
   1
   1
   1

Table aliases are supported in short form i.e without AS

.. code-block:: bash

   OCTO> select n1.firstname from names "n1" limit 1;
   Zero

.. note::
   * If single quotes or double quotes are used, keywords like NULL, AS etc can be used as alias name

   * Aliasing with quoted multi words, containing spaces, are supported. But their usage as a reference (column or table) is not yet supported

     For example:

             Supported:

                 select id **as "id a"** from names;

                 select id from names **as "n one"**;

                 select id **"id a"** from names;

                 select id from names **"n one"**;

             Not Supported:

                 select **"id a"** from (select 8 as "id a") n1; -> **(column name with spaces)**

                 select 1 from names as n1 inner join (select n2.id from names as n2 LIMIT 3) as "alias two" ON (n1.id = **"alias two".id**); -> **(table name with spaces)**

   * Multi word aliases i.e with spaces can only be formed with single or double quotes

     For example:

             Supported:

                 column **[AS] "word word"**

                 column **[AS] 'word word'**

                 [table_name | subquery] **[AS] "word word"**

                 [table_name | subquery] **[AS] 'word word'**

             Not supported:

                 column **[AS] word word**

                 [table_name | subquery] **[AS] word word**

------------------------
Pattern Processing
------------------------

+++++++++++
LIKE
+++++++++++

.. code-block:: SQL

   string LIKE pattern

If the pattern matches the string, LIKE operation returns true.

Pattern is expected to match the entire string i.e.

.. code-block:: SQL

   'a'  LIKE 'a' -> TRUE
   'ab' LIKE 'a' -> FALSE

:code:`%` and :code:`_` have a special meaning.
:code:`%` matches any string of zero or more characters and :code:`_` matches any single chracter.

.. code-block:: SQL

   'abcd' LIKE '%'    -> TRUE
   'abcd' LIKE 'ab%'  -> TRUE
   'cdcd' LIKE 'ab%'  -> FALSE
   'abcd' LIKE 'a_cd' -> TRUE
   'ebcd' LIKE 'a_cd' -> FALSE

Escaping :code:`%` or :code:`_` will take away its special meaning, and, it will just match :code:`%` and :code:`_` in its literal form.

.. code-block:: SQL

   'ab%ab' LIKE 'ab\%ab' -> TRUE
   'abab'  LIKE 'ab\%ab' -> FALSE
   'ab_ab' LIKE 'ab\_ab' -> TRUE
   'abab'  LIKE 'ab\_ab' -> FALSE

To match an escape as itself additional escape is required. Any other character if escaped has no special meaning. It will match its literal self.

.. code-block:: SQL

   'ab\ab' LIKE 'ab\\ab' -> TRUE
   'ab\ab' LIKE 'ab\ab'  -> FALSE
   'abab'  LIKE 'ab\ab'  -> TRUE

Any other character is matched without any special meaning.

.. code-block:: SQL

   'ab*&$#' LIKE 'ab*&$#' -> TRUE
   'ab*&$#' LIKE 'ab*'    -> FALSE

~~~~~~~~~~~~~~~~~~~~~~~
Variations of LIKE
~~~~~~~~~~~~~~~~~~~~~~~

#. :code:`~~` : Same as LIKE

#. :code:`ILIKE` : Case insensitive version of LIKE

   .. code-block:: SQL

      'abc' ILIKE 'Abc' -> TRUE
      'abc' LIKE  'Abc' -> FALSE

#. :code:`~~*` : Case insensitive version of LIKE

#. :code:`NOT LIKE` : Negated version of LIKE

   .. code-block:: SQL

     'abc' LIKE 'abc'      -> TRUE
     'abc' LIKE 'cba'      -> FALSE
     'abc' LIKE '%'        -> TRUE
     'abc' NOT LIKE 'abc'  -> FALSE
     'abc' NOT LIKE 'cba'  -> TRUE
     'abc' NOT LIKE '%'    -> FALSE

#. :code:`!~~` : Negated version of LIKE

#. :code:`NOT ILIKE` : Negated version of case insensitive LIKE

#. :code:`!~~*` : Negated version of case insensitive LIKE

~~~~~~~~~~~~~
Error Case
~~~~~~~~~~~~~
LIKE pattern cannot end with an escape character. This results in an error.

.. code-block:: bash

   'abc' LIKE 'abc\'
   [ERROR] PATH:LINENUM DATE TIME : Cannot end pattern with escape character: abc\

   'abc\' LIKE 'abc\\' -> TRUE


+++++++++++++++++++
SIMILAR TO
+++++++++++++++++++

.. code-block:: SQL

   string SIMILAR TO pattern

If the pattern matches the string, SIMILAR TO operation returns true.

Pattern is expected to match the entire string i.e.

.. code-block:: SQL

   'a'  SIMILAR TO 'a' -> TRUE
   'ab' SIMILAR TO 'a' -> FALSE

As seen in the :code:`LIKE` operation, following characters have special meaning:

* :code:`%` matches any string of zero or more characters
* :code:`_` matches any single character
* Escaping :code:`%` or :code:`_` will take away its special meaning, and, it will just match :code:`%` or :code:`_` in its literal form
* To match an escape as itself additional escape is required

Additionally, the following characters also having special meaning:

* :code:`|` : The whole string should match a unit on either side of :code:`|`

  .. code-block:: SQL

     'abd' SIMILAR TO 'abc|d'       -> TRUE ( Here along with other characters, the right side of | which is 'd' is matched )
     'dba' SIMILAR TO '(abc)|(dba)' -> TRUE ( Here the right side of | which is (dba) is matched )

* :code:`*` : Match a sequence of zero or more units

  .. code-block:: SQL

     'wow'         SIMILAR TO 'woo*w'    -> TRUE
     'wooow'       SIMILAR TO 'woo*w'    -> TRUE
     'dabcabcabcd' SIMILAR TO 'd(abc)*d' -> TRUE
     'dd'          SIMILAR TO 'd(abc)*d' -> TRUE

* :code:`+` : Match a sequence of one or more units

  .. code-block:: SQL

     'dabcabcd' SIMILAR TO 'd(abc)+d'  -> TRUE
     'dd'       SIMILAR TO 'd(abc)+d'  -> FALSE

* :code:`( )` : Groups contained items into a single logical unit

* :code:`[ ]` : Matches any one of the characters mentioned inside the brackets

  .. code-block:: SQL

     'a' SIMILAR TO '[abc]' -> TRUE
     'c' SIMILAR TO '[abc]' -> TRUE
     'd' SIMILAR TO '[abc]' -> FALSE

* :code:`{ }`

  * :code:`{m}` : Match a sequence of exactly *m* units

    .. code-block:: SQL

       'aaaa' SIMILAR TO 'a{4}' -> TRUE
       'aaa'  SIMILAR TO 'a{4}' -> FALSE

  * :code:`{m,}` : Match a sequence of *m* or more units

    .. code-block:: SQL

       'aaaaa'  SIMILAR TO 'a{2,}'      -> TRUE
       'a'      SIMILAR TO 'a{2,}'      -> FALSE
       'ababab' SIMILAR TO '(ab){2,}'   -> TRUE
       'ab'     SIMILAR TO '(ab){2,}'   -> FALSE

  * :code:`{m,n}` : Match a sequence of exactly *m* through *n* (inclusive) units

    .. code-block:: SQL

       'aaa' SIMILAR TO 'a{1,3}'   -> TRUE
       'aa'  SIMILAR TO 'a{1,3}'   -> FALSE

* :code:`?` : Match zero or one unit

  .. code-block:: SQL

     'abc'  SIMILAR TO 'ab?c'    -> TRUE
     'ac'   SIMILAR TO 'ab?c'    -> TRUE
     'abbc' SIMILAR TO 'ab?c'    -> FALSE
     'azyc' SIMILAR TO 'a(zy)?c' -> TRUE
     'ac'   SIMILAR TO 'a(zy)?c' -> TRUE
     'azc'  SIMILAR TO 'a(zy)?c' -> FALSE

.. note::
   * A **unit** refers to a logical grouping done using ( ) or a character depending on its usage

     For example:

             'ababab' SIMILAR TO '(ab)+' -> TRUE ( Here ab is the logical unit considered by + )

             'abbb' SIMILAR TO 'ab+'     -> TRUE ( Here b is the logical unit considered by + )

   * Similar to the LIKE operation, if the above characters are escaped they lose their special meaning


~~~~~~~~~~~~~~~~~~~~~~~~~~~
Variation of SIMILAR TO
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#. :code:`NOT SIMILAR TO` : Negated version of SIMILAR TO

   .. code-block:: SQL

      'abc' SIMILAR TO     'abc'   -> TRUE
      'abc' NOT SIMILAR TO 'abc'   -> FALSE

+++++++++++++++++++++
 TILDE ~
+++++++++++++++++++++

.. code-block:: SQL

   string ~ pattern

If the pattern matches the string, ~ operation returns true.

Partial match of the pattern is valid, i.e.

.. code-block:: SQL

   'a'  ~ 'a'          -> TRUE
   'ab' ~ 'a'          -> TRUE  (Partial match is valid)
   'ab' SIMILAR TO 'a' -> FALSE (Partial match is not valid)
   'ab' LIKE 'a'       -> FALSE (Partial match is not valid)

:code:`%` and :code:`_` have no special meaning. They are matched as literals.

To match an escape as itself additional escape is required.

The following characters have special meaning:

* :code:`.` : Matches any single character

  .. code-block:: SQL

     'abc' ~ '...' -> TRUE

* :code:`*` : Match a sequence of zero or more units

  .. code-block:: SQL

     'aab' ~ 'a*'  -> TRUE
     'baa' ~ 'a*'  -> TRUE

* :code:`|` : Match a unit on either side of :code:`|`

  .. code-block:: SQL

     'abd' LIKE       'abc|d'       -> FALSE ( | does not have special meaning for LIKE operation )
     'abd' SIMILAR TO 'abc|d'       -> FALSE ( | expects 'abd' to match either 'abc' or 'd' . But, as 'abd' is not either of those, the result is FALSE )
     'abd' ~          'abc|d'       -> TRUE  ( | expects 'abd' to match either 'abc' or 'abd'. Hence the result is TRUE )

* :code:`+` : Match a sequence of one or more units

  .. code-block:: SQL

     'dabcabcd' ~ '(abc)+'  -> TRUE
     'dd'       ~ '(xyz)+'  -> FALSE
     'dd'       ~ 'd+'      -> TRUE
     'a'        ~ 'd+'      -> FALSE

* :code:`( )` : Groups contained items into a single logical unit

* :code:`[ ]` : Matches any one of the characters mentioned inside the brackets

  .. code-block:: SQL

     'a'   ~ '[abc]' -> TRUE
     'zay' ~ '[abc]' -> TRUE
     'zy'  ~ '[abc]' -> FALSE

* :code:`{ }`

  * :code:`{m}` : Match a sequence of exactly *m* units

    .. code-block:: SQL

       'yyaaaabcc' ~ 'a{4}' -> TRUE
       'yyaaabcc'  ~ 'a{4}' -> FALSE

  * :code:`{m,}` : Match a sequence of *m* or more units

    .. code-block:: SQL

       'yyaaabcc'     ~ 'a{2,}'      -> TRUE
       'yyabcc'       ~ 'a{2,}'      -> FALSE
       'yyabaaababcc' ~ '(ab){2,}'   -> TRUE
       'yyabcc'       ~ '(ab){2,}'   -> FALSE

  * :code:`{m,n}` : Match a sequence of exactly *m* through *n* (inclusive) units

    .. code-block:: SQL

       'aaa' ~ 'a{1,3}'   -> TRUE
       'aa'  ~ 'a{1,3}'   -> FALSE

* :code:`?` : Match zero or one unit

  .. code-block:: SQL

     'abcd'  ~ 'ab?c'    -> TRUE
     'acd'   ~ 'ab?c'    -> TRUE
     'abbcd' ~ 'ab?c'    -> FALSE
     'azycd' ~ 'a(zy)?c' -> TRUE
     'acd'   ~ 'a(zy)?c' -> TRUE
     'azcd'  ~ 'a(zy)?c' -> FALSE

.. note::
   * A **unit** refers to a logical grouping done using ( ) or a character depending on its usage

   * If the above characters are escaped they lose their special meaning

~~~~~~~~~~~~~~~~~~~~
Variations of ~
~~~~~~~~~~~~~~~~~~~~

#. :code:`!~` : Negated version of ~

#. :code:`~*` : Case insensitive version of ~

#. :code:`!~*` : Negated version of case insensitive ~

.. _technical-notes:

---------------------
Technical Notes
---------------------

The following rule for a row_value_constructor is currently a deviation from BNF due to a Reduce-Reduce conflict in the grammar:

.. code-block:: none

   row_value_constructor : [(][value_expression | null_specification | default_specification] [, ....][)];

A primary value expression is denoted as follows:

.. code-block:: none

   value_expression: unsigned_value_specification | column_reference | COUNT (\*|[set_quantifier] value_expression) | general_set_function | scalar_subquery | (value_expression);

The value expression can contain an unsigned value, a column reference, a set function or a subquery.

general_set_function refers to functions on sets like AVG, SUM, MIN, MAX etc. A set function can also contain the keyword COUNT, to count the number of resulting columns or rows that result from the query.

A query expression can be a joined table or a non joined query expression.

.. code-block:: none

   query_expression: non_join_query_expression | joined_table;

The non_join_query_expression includes simple tables and column lists.

---------------------
DDL Example
---------------------

The following is a sample of a DDL for an existing large M application (a healthcare information system) which was generated automatically from the application schema.

.. code-block:: SQL

   CREATE TABLE `ORDER_ORDER_ACTIONS`(
    `ORDER1_ID` INTEGER PRIMARY KEY START 0 END "'(keys(""ORDER1_ID""))!(keys(""ORDER1_ID"")="""")",
    `ORDER_ORDER_ACTIONS_ID` INTEGER KEY NUM 1 START 0 END "'(keys(""ORDER_ORDER_ACTIONS_ID""))!(keys(""ORDER_ORDER_ACTIONS_ID"")="""")",
    `DATE_TIME_ORDERED` INTEGER NOT NULL GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 1,
    `REASON_FOR_ACTION_REJECT` CHARACTER(240) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),1)" PIECE 1,
    `ACTION` CHARACTER(12) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 2,
    `PROVIDER` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 3,
    `SIGNATURE_STATUS` CHARACTER(34) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 4,
    `SIGNED_BY` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 5,
    `DATE_TIME_SIGNED` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 6,
    `SIGNED_ON_CHART` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 7,
    `VERIFYING_NURSE` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 8,
    `DATE_TIME_NURSE_VERIFIED` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 9,
    `VERIFYING_CLERK` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 10,
    `DATE_TIME_CLERK_VERIFIED` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 11,
    `NATURE_OF_ORDER` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 12,
    `ENTERED_BY` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 13,
    `TEXT_REFERENCE` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 14,
    `RELEASE_STATUS` CHARACTER(11) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 15,
    `RELEASE_DATE_TIME` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 16,
    `RELEASING_PERSON` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 17,
    `CHART_REVIEWED_BY` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 18,
    `DATE_TIME_CHART_REVIEWED` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),0)" PIECE 19,
    `DC_HOLD_UNTIL` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),2)" PIECE 1,
    `DC_HOLD_RELEASED_BY` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),2)" PIECE 2,
    `DIGITAL_SIGNATURE` CHARACTER(100) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),2)" PIECE 3,
    `DRUG_SCHEDULE` CHARACTER(3) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),2)" PIECE 4,
    `DIGITAL_SIGNATURE_REQUIRED` CHARACTER(3) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),2)" PIECE 5,
    `FLAGGED` CHARACTER(3) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),3)" PIECE 1,
    `BULLETIN` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),3)" PIECE 2,
    `DATE_TIME_FLAGGED` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),3)" PIECE 3,
    `FLAGGED_BY` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),3)" PIECE 4,
    `REASON_FOR_FLAG` CHARACTER(80) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),3)" PIECE 5,
    `DATE_TIME_UNFLAGGED` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),3)" PIECE 6,
    `UNFLAGGED_BY` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),3)" PIECE 7,
    `REASON_FOR_UNFLAG` CHARACTER(80) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),3)" PIECE 8,
    `ALERTED_PROVIDER` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),3)" PIECE 9,
    `DISPOSITION_BY` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),4)" PIECE 1,
    `DISPOSITION_DATE_TIME` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),4)" PIECE 2,
    `CHART_COPY_PRINTED` CHARACTER(3) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),7)" PIECE 1,
    `CHART_COPY_PRINTED_WHEN` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),7)" PIECE 2,
    `CHART_COPY_PRINTED_BY` INTEGER GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),7)" PIECE 3,
    `CHART_COPY_PRINTER` CHARACTER(50) GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""),7)" PIECE 4
   )
   GLOBAL "^OR(100,keys(""ORDER1_ID""),8,keys(""ORDER_ORDER_ACTIONS_ID""))"
   DELIM "^";

* The backtick character (`) is used to enclose words so that any possible reserved words that may be used in column or table names are correctly escaped.

* START indicates where to start a $ORDER loop in the underlying data storage - this is the number BEFORE which actual data needs to be returned.

* END is an M condition that indicates when the $ORDER loop should stop looking for data. When END is used in the third line of the above example, for instance, it is looking for two different conditions: if keys("ORDER1_ID") is false OR if keys(ORDER1_ID) is the empty string.

* The NUM keyword identifies the order in which multiple KEYS are ordered. This also indicates that this column is derived from subscripts of the M global reference (key) vs data contained within the subscript (value).

* The PIECE keyword indicates which M piece the data resides in.

* The DELIM keyword defines the delimiter for data stored within a global node (value) and used in conjunction with the PIECE keyword to access data specified in the column definitions.

.. note::
   When parsed, if a table and a column have the same name, a query will give preference to the table name over the derived column name.
