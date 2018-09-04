
================
DBMS Grammar
================

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

An optional_keyword can be added to further enhance the CREATE statement. The following keywords are MUMPS command-expressions and literals:

.. parsed-literal::
   [CURSOR | DELIM | END | EXTRACT | GLOBAL | PACK | PIECE | START | UNPACK LITERAL]

* CURSOR LITERAL is a table keyword used to increment the cursor by one element.
* DELIM LITERAL is a table/column keyword that represents the "PIECE" string to be used in `$PIECE <https://docs.yottadb.com/ProgrammersGuide/functions.html#piece>`_. If present in a column, it overrides the table/default DELIM setting.
* END LITERAL is a table keyword which indicates that the cursor has hit the last record in the table.
* EXTRACT LITERAL is a column keyword that extracts the value of the column from the database.
* GLOBAL LITERAL is a table/column keyword representing the "source" location for a table.
* PACK LITERAL is a table keyword that packs the cursor into a single value to be stored in a global specified by the SQL engine.
* PIECE LITERAL is a column keyword representing the $PIECE number of the row this column refers to - if present, it overrides the default (which is the column number, starting at 1).  
* START LITERAL is a table keyword that defines the local variables when a cursor first begins running.
* UNPACK LITERAL is a table keyword that unpacks the cursor from a global filled in by the SQL engine.

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
   SELECT [ALL | DISTINCT] ASTERISK | column[...,column] FROM table_name [WHERE search_condition] [GROUP BY column[,..column]] [HAVING search_condition] [ORDER BY sort_specification];

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

The UPDATE statement begins with the keyword UPDATE, along with specifying the table_name to be updated and the keyword SET, followed by a list of comma-separated statements that are used to update the existing columns where the object_column is a particular column and the update_source what the value at that column is set to either NULL or a specific value expression. The optional WHERE condition allows you to update columns based on a certain condition you specify.

Example:

.. parsed-literal::
   UPDATE Employee SET FirstName = "John" WHERE ID = 220; 


------------
DELETE
------------

.. parsed-literal::
   DELETE FROM table_name [WHERE search_condition];

The delete statement consists of the keywords DELETE FROM followed by the name of the table and possibly a search condition.

The search condition eventually yields a boolean true or false value, and may contain further search modifications detailing where to look to apply the search_condition and how to compare the resulting values.

Example:

.. parsed-literal::
   DELETE FROM Employee WHERE ID = 220;

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
