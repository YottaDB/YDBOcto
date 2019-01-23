
================
DBMS Grammar
================

.. contents::
   :depth: 4

A statement in SQL is either a schema statement, a data statement or a SELECT statement.

A schema statement creates and manipulates a unique schema within the database.

A data statement is any statement that makes a change to the data in the database. Changes to data can be brought about by deleting data, inserting new data or updating existing data.

A SELECT statement is used to select and view data from the database.

---------------
CREATE
---------------

.. parsed-literal::
   CREATE TABLE table_name (column_name data_type [constraints][, ... column_name data_type [constraints]]) [optional_keyword];

The CREATE statement is used to create tables in the database. The keywords CREATE TABLE are used followed by the name of the table to be created.

The names of columns to be created in the database and their datatypes are then specified in a list, along with any constraints that might need to apply (such as denoting a PRIMARY KEY, UNIQUE KEY or FOREIGN KEY).

Example:

.. parsed-literal::
   CREATE TABLE Employee (ID int PRIMARY KEY, FirstName char(20), LastName char(30));


+++++++++++++++++++++++++++++++++++++++++++++
Mapping to existing YottaDB global variables
+++++++++++++++++++++++++++++++++++++++++++++

If mapping to existing YottaDB global variables, an optional_keyword can be added to further enhance the CREATE statement:

.. parsed-literal::
   [ADVANCE | CURSOR | DELIM | END | EXTRACT | GLOBAL | KEY NUM | PACK | PIECE | UNPACK LITERAL]

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
| PACK                           | Command expression            | Table                  | Packs the cursor into a single value to be stored in a global                  | \-                           | SET storeKey=$$STOREKEY("cur |
|                                |                               |                        | specified by the SQL engine.                                                   |                              | sor_name",.keys),@storeKey   |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| PIECE                          | Literal                       | Column                 | Represents the                                                                 | default (column number,      | \-                           |
|                                |                               |                        | `$PIECE <https://docs.yottadb.com/ProgrammersGuide/functions.html#piece>`_     | starting at 1)               |                              | 
|                                |                               |                        | number of the row this column refers to                                        |                              |                              |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+
| UNPACK                         | Command expression            | Table                  | Unpacks the cursor from a global filled in by the SQL engine                   | \-                           | SET cursor="cursor_name",    |
|                                |                               |                        |                                                                                |                              | keys(0)=$P($G(@cursor),      |
|                                |                               |                        |                                                                                |                              | "|",1)                       |
+--------------------------------+-------------------------------+------------------------+--------------------------------------------------------------------------------+------------------------------+------------------------------+

In the table above:

* table_name and cursor_name are variables representing the names of the table and the cursor being used.
* keys is a special local variable that contains all the keys used by the table.
* $$STOREKEY is an M routine that converts an array of keys (keys) to a format that can be stored in the cursor as ^cursor(sessionId,keys(0),keys(1)).
* @storeKey is an M-expression which does indirection on the value stored in storeKey.
* @cursor does indirection on the cursor variable.
* $P represents `$PIECE <https://docs.yottadb.com/ProgrammersGuide/functions.html#piece>`_, which is a function that returns a logical field from a logical record.
* $G represents `$GET <https://docs.yottadb.com/ProgrammersGuide/functions.html#get>`_ , an M function that returns the value of a local or global variable if the variable has a value.

-----------------
DROP
-----------------

.. parsed-literal::
   DROP TABLE table_name [CASCADE | RESTRICT];

The DROP statement is used to remove tables from the database. The keywords DROP TABLE are followed by the name of the table desired to be dropped. Optional parameters include CASCADE and RESTRICT.

The CASCADE parameter is used to specify that all objects depending on the table will also be dropped.

The RESTRICT parameter is used to specify that the table referred to by table_name will not be dropped if there are existing objects depending on it.

Example:

.. parsed-literal::
   DROP TABLE Employee CASCADE;

-----------
SELECT
-----------

.. parsed-literal::
   SELECT [ALL | DISTINCT] ASTERISK | column[...,column] FROM table_name [WHERE search_condition] [GROUP BY column[,..column]] [HAVING search_condition] [ORDER BY sort_specification] [LIMIT number];

The SELECT statement is used to select rows from the database by specifying a query, and optionally sorting the resulting rows.

- ALL : returns all values
- DISTINCT: returns on different (non-duplicate) values

FROM denotes the table from which the columns are selected.

The WHERE clause represents a condition under which columns are selected.

The GROUP BY clause ensures that the resulting rows are grouped together by certain characteristics.

The HAVING clause works to filter the rows that result from the GROUP BY clause.

Example:

.. parsed-literal::
   SELECT ID, FirstName, LastName FROM Employee WHERE ID > 100 GROUP BY LastName;

The LIMIT clause allows the user to specify the number of rows they want to retrieve from the results of the query.

Example:

.. parsed-literal::
   SELECT * FROM Employee LIMIT 5;

The above example returns no more than 5 rows.

++++++++
Sorting
++++++++

ORDER BY lets you sort the order of the rows returned after the query.

To sort rows or columns in the database, you need to have the following sort_specification.

.. parsed-literal::
   sort_key [COLLATE collation_name] [ASC | DESC];

The sort_key is either a column_reference or a literal.

The sort key can be followed by a collate clause, ordering specification or both.

.. note::
   A collation is a set of rules to compare characters in a character set.

The collate clause consists of the word COLLATE and the relevant collation name.

The ordering specification lets you further choose to order the returned columns in either ascending (ASC) or descending (DESC) order.

Example:

.. parsed-literal::
   SELECT ID, FirstName, LastName FROM Employee WHERE ID > 100 ORDER BY ID DESC;

++++++
Joins
++++++

Joins can be made by appending a join table to a SELECT statement:

.. parsed-literal::
   [CROSS | [NATURAL | INNER | [LEFT][RIGHT][FULL] OUTER]] JOIN ON joined_table;

A cross join between two tables provides the number of rows in the first table multiplied by the number of rows in the second table.

A qualified join is a join between two tables that specifies a join condition.

join_specification allows the user to specify a condition for the table join.

A NATURAL JOIN is a JOIN operation that creates an implicit join clause for you based on the common columns in the two tables being joined.

Types of Joins:

- Inner Join : Only the common columns between the two tables are returned.
- Outer Join
  - Left Outer Join : All the columns of the left table are returned, along with matching columns of the right table.
  - Right Outer Join: Matching columns of the left table are returned, along with all the columns of the right table.
  - Full Outer Join: All columns from both tables are returned.

Example:

.. parsed-literal::
   SELECT FirstName, LastName, Address FROM Employee INNER JOIN Addresses ON Employee.ID = Addresses.EID; 

--------------
INSERT
--------------

.. parsed-literal::
   INSERT INTO table_name ( column name [, column name ...]) [ VALUES ... | (SELECT ...)];

The INSERT statement allows you to insert values into a particular column. These can either be default values or values specified by the query expression i.e. the result of a SELECT statement.

Example:

.. parsed-literal::
   INSERT INTO Employee (ID , FirstName, LastName) [220, "Jon", "Doe"];

--------------
UPDATE
--------------

.. parsed-literal::
   UPDATE table_name SET object_column EQUALS update_source [WHERE search_condition];

The UPDATE statement begins with the keyword UPDATE. The table_name to be updated and the keyword SET is followed by a list of comma-separated statements that are used to update the existing columns, where object_column is a particular column and update_source is set to either NULL or a specific value expression. The optional WHERE condition allows you to update columns based on a certain condition you specify.

Example:

.. parsed-literal::
   UPDATE Employee SET FirstName = "John" WHERE ID = 220; 

------------
DELETE
------------

.. parsed-literal::
   DELETE FROM table_name [WHERE search_condition];

The DELETE statement consists of the keywords DELETE FROM followed by the name of the table and possibly a search condition.

The search condition eventually yields a boolean true or false value, and may contain further search modifications detailing where to apply the search_condition and how to compare the resulting values.

Example:

.. parsed-literal::
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

.. parsed-literal::
   SELECT [.....] FROM table_name[...]  UNION [ALL] SELECT [.....] FROM table_name2[...]....;

The UNION operation consists of two or more queries joined together with the word UNION.  It combines the results of two individual queries into a single set of results.

The keyword ALL ensures that duplicate rows of results are not removed during the UNION.

Example:

.. parsed-literal::
   SELECT FirstName FROM Employee UNION SELECT FirstName FROM AddressBook;

++++++++++++++++
INTERSECT
++++++++++++++++

.. parsed-literal::
   SELECT [.....] FROM table_name[......] INTERSECT [ALL] SELECT [.....] FROM table_name2[....]......;

The INTERSECT operation consists of two or more queries joined together with the word INTERSECT. It returns distinct non-duplicate results that are returned by both queries on either side of the operation.

The keyword ALL ensures that duplicate rows of results returned by both queries are not eliminated during the INTERSECT.

.. parsed-literal::
   SELECT ID FROM Employee INTERSECT SELECT ID FROM AddressBook;

++++++++++++++
EXCEPT
++++++++++++++

.. parsed-literal::
   SELECT [.....] FROM table_name[.....] EXCEPT [ALL] SELECT [.....] FROM table_name2[......].......;

The EXCEPT operation consists of two or more queries joined together with the word EXCEPT. It returns (non-duplicate) results from the query on the left side except those that are also part of the results from the query on the right side.

The keyword ALL affects the resulting rows such that duplicate results are allowed but rows in the first table are eliminated if there is a corresponding row in the second table.

.. parsed-literal::
   SELECT LastName FROM Employee EXCEPT SELECT LastName FROM AddressBook;

-------------
Other
-------------

The following rule for a row_value_constructor is currently a deviation from BNF due to a Reduce-Reduce conflict in the grammar:

.. parsed-literal::
   row_value_constructor : [(][value_expression | null_specification | default_specification] [, ....][)];

A primary value expression is denoted as follows:

.. parsed-literal::
   value_expression: unsigned_value_specification | column_reference | COUNT (\*|[set_quantifier] value_expression) | general_set_function | scalar_subquery | (value_expression);

The value expression can contain an unsigned value, a column reference, a set function or a subquery.

general_set_function refers to functions on sets like AVG, SUM, MIN, MAX etc. A set function can also contain the keyword COUNT, to count the number of resulting columns or rows that result from the query.

A query expression can be a joined table or a non joined query expression.

.. parsed-literal::
   query_expression: non_join_query_expression | joined_table;

The non_join_query_expression includes simple tables and column lists.

The comparative operators are:

* EQUALS =
* NOT EQUALS <>
* LESS THAN <
* GREATER THAN >
* LESS THAN OR EQUALS <=
* GREATER THAN OR EQUALS >=
