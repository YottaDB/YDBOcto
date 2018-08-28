
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
   delete_statement_searched: DELETE FROM table_name [WHERE search_condition];

The delete statement consists of the keywords DELETE FROM followed by the name of the table and possibly a search condition.

The search condition eventually yields a boolean true or false value, and may contain further search modifications detailing where to look to apply the search_condition and how to compare the resulting values.

-------------
Other
-------------

The following rule is currently a deviation from BNF due to a Reduce-Reduce conflict in the grammar:

.. parsed-literal::
   row_value_constructor : [(]row_value_constructor_element [, row_value_constructor_element ...][)];

A row_value_constructor consists of a comma-separated list of row value constructor elements:

.. parsed-literal::
   row_value_constructor_element: value_expression | null_specification | default_specification;

A Primary Value Expression is denoted as follows:

.. parsed-literal::
   value_expression_primary 
   : unsigned_value_specification 
   | column_reference 
   | set_function_specification 
   | scalar_subquery 
   | (value_expression);

The primary value expression can either contain an unsigned value, a column reference, a set function or a subquery.

.. parsed-literal::
   set_function_specification : COUNT ( * | [set_quantifier] value_expression) | general_set_function;

general_set_function refers to functions on sets like AVG, SUM, MIN, MAX etc. A set function can also contain the keyword COUNT, to count the number of resulting columns or rows that result from the query.

A query expression can be a joined table or a non joined query expression.

.. parsed-literal::
   query_expression
  : non_join_query_expression
  | joined_table
  ;

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
   sql_select_statement: query_specification [SORT BY sort_specification];

The SELECT statement is used to select rows from the database by specifying a query, and optionally sorting the resulting rows.

---------
Sorting
---------

To sort rows or columns in the database, you need to specify a key on which the rows can be sorted, potentially followed by a collate clause and/or ordered in ascending or descending order..

.. parsed-literal::
   sort_specification: sort_key [collate_clause] [ordering_specification];

The specification has to have a key by which the rows or columns are identified:

.. parsed-literal::
   sort_key : column_reference | UNSIGNED_INTEGER;

The sort key can be followed by a collate clause, ordering specification or both.

.. note::
   A collation is a set of rules to compare characters in a character set.

The collate clause consists of the word COLLATE and the relevant collation name.

.. parsed-literal::
   collate_clause: COLLATE collation_name;

You can choose to order the returned columns in either the ascending or descending order.

.. parsed-literal::
   ordering_specification: ASC|DESC;

--------------------
Query Specification
--------------------

.. parsed-literal::
   query_specification: SELECT [set_quantifier] select_list table_expression [ORDER BY sort_specification_list];

A query is specified by using the SELECT keyword in conjunction with lists, expressions and quantifiers.

.. parsed-literal::
   select_list: ASTERISK | select_sublist;

The select_list can either be an asterisk (\*) or a list of columns.

.. parsed-literal::
   select_sublist: derived_column | derived_column select_sublist_tail;

The list of columns, or sublist consists of comma-separated elements.

.. parsed-literal::
   select_sublist_tail: COMMA select_sublist;

A derived column is made up of alpha-numeric expressions.

The set_quantifier can either be:

- ALL : returns all values
- DISTINCT: returns on different (non-duplicate) values

ORDER BY denotes the order in which columns are returned after the query.

The table expression consists of multiple clauses:

.. parsed-literal::
   table_expression
  : from_clause where_clause group_by_clause having_clause;

The from_clause represents the table from which the columns are selected.

The where_clause represents a condition under which columns are selected.

The group_by clause ensures that the resulting columns are grouped together by certain characteristics and the having_clause works to filter the columns that result from the group_by clause.

-----
Joins
-----

A joined table consists of the following:

.. parsed-literal::
   joined_table : [CROSS | [NATURAL | INNER | [LEFT][RIGHT][FULL] OUTER]] JOIN ON joined_table;

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
   insert_statement: INSERT INTO table_name ( column name [, column name ...]) [ VALUES ... | (SELECT ...)];

The INSERT statement allows you to insert values into a particular column. These can either be default values or values specified by the query expression i.e. the result of a SELECT statement.

===============
UPDATE
===============

.. parsed-literal::
   update_statement_searched: UPDATE table_name SET object_column EQUALS update_source [WHERE search_condition];

The UPDATE statement begins with the keyword UPDATE, along with specifying the table_name to be updated and the keyword SET, followed by a list of comma-separated statements that are used to update the existing columns where the object_column is a particular column and the update_source what the value at that column is set to either NULL or a specific value expression. The optional WHERE condition allows you to update columns based on a certain condition you specify.

================
DROP
================

.. parsed-literal::
   drop_table_statement: DROP TABLE table_name [CASCADE | RESTRICT];

The DROP statement is used to remove tables from the database. The keywords DROP TABLE are followed by the name of the table desired to be dropped. Optional parameters include CASCADE and RESTRICT.

The CASCADE parameter is used to specify that all objects depending on the table will also be dropped.
The RESTRICT parameter is used to specify that the table referred to by table_name will not be dropped if there are existing objects depending on it.
