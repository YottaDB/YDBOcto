
================
DBMS Grammar
================

.. parsed-literal::
   sql_statement: sql_schema_statement | sql_data_statement | sql_select_statement;

A SQL statement consists of either a Schema statement, a Data statement or a SELECT statement.

A Data Statement is a statement that makes a change to the data.

.. parsed-literal::
   sql_data_statement : sql_data_change_statement

The changes to data can be brought about by deleting data, inserting new data or updating existing data.

.. parsed-literal::
   sql_data_change_statement: delete_statement_searched | insert_statement | update_statement_searched;

==========
DELETE
==========

.. parsed-literal::
   DELETE FROM table_name [WHERE search_condition];

The delete statement consists of the keywords DELETE FROM followed by the name of the table and possibly a search condition.

The search condition eventually yields a boolean true or false value, and may contain further search modifications detailing where to look to apply the search_condition and how to compare the resulting values.

-------------
Other
-------------

The following rule is currently a deviation from BNF due to a Reduce-Reduce conflict in the grammar:

.. parsed-literal::
   row_value_constructor : [(][value_expression|null_specification|default_specification] [, ....][)];

A Primary Value Expression is denoted as follows:

.. parsed-literal::
   unsigned_value_specification | column_reference | COUNT ( \*|[set_quantifier] value_expression)|general_set_function | scalar_subquery | (value_expression);

The primary value expression can either contain an unsigned value, a column reference, a set function or a subquery.

general_set_function refers to functions on sets like AVG, SUM, MIN, MAX etc. A set function can also contain the keyword COUNT, to count the number of resulting columns or rows that result from the query.

A query expression can be a joined table or a non joined query expression.

.. parsed-literal::
   query_expression: non_join_query_expression|joined_table;

The non_join_query_expression includes simple tables and column lists.

The comparative operators are:

* EQUALS =
* NOT EQUALS <>
* LESS THAN <
* GREATER THAN >
* LESS THAN OR EQUALS <=
* GREATER THAN OR EQUALS >=

=====================
SELECT
=====================

.. parsed-literal::
   SELECT [ALL|DISTINCT] ASTERISK|column[...,column] from_clause where_clause group_by_clause having_clause [ORDER BY sort_specification_list] [SORT BY sort_specification];

The SELECT statement is used to select rows from the database by specifying a query, and optionally sorting the resulting rows.

- ALL : returns all values
- DISTINCT: returns on different (non-duplicate) values

The from_clause represents the table from which the columns are selected.

The where_clause represents a condition under which columns are selected.

The group_by clause ensures that the resulting columns are grouped together by certain characteristics and the having_clause works to filter the columns that result from the group_by clause.

ORDER BY denotes the order in which columns are returned after the query.

---------
Sorting
---------

To sort rows or columns in the database, you need to specify a key on which the rows can be sorted, potentially followed by a collate clause and/or ordered in ascending or descending order..

.. parsed-literal::
   column_reference|UNSIGNED_INTEGER [COLLATE collation_name] [ASC|DESC];

The sort key is either a reference to a column or an unsigned integer.

The sort key can be followed by a collate clause, ordering specification or both.

.. note::
   A collation is a set of rules to compare characters in a character set.

The collate clause consists of the word COLLATE and the relevant collation name.

You can choose to order the returned columns in either the ascending or descending order.

-----
Joins
-----

A joined table consists of the following:

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


================
INSERT
================

.. parsed-literal::
   INSERT INTO table_name ( column name [, column name ...]) [ VALUES ... | (SELECT ...)];

The INSERT statement allows you to insert values into a particular column. These can either be default values or values specified by the query expression i.e. the result of a SELECT statement.

===============
UPDATE
===============

.. parsed-literal::
   UPDATE table_name SET object_column EQUALS update_source [WHERE search_condition];

The UPDATE statement begins with the keyword UPDATE, along with specifying the table_name to be updated and the keyword SET, followed by a list of comma-separated statements that are used to update the existing columns where the object_column is a particular column and the update_source what the value at that column is set to either NULL or a specific value expression. The optional WHERE condition allows you to update columns based on a certain condition you specify.

================
DROP
================

.. parsed-literal::
   DROP TABLE table_name [CASCADE | RESTRICT];

The DROP statement is used to remove tables from the database. The keywords DROP TABLE are followed by the name of the table desired to be dropped. Optional parameters include CASCADE and RESTRICT.

The CASCADE parameter is used to specify that all objects depending on the table will also be dropped.
The RESTRICT parameter is used to specify that the table referred to by table_name will not be dropped if there are existing objects depending on it.
