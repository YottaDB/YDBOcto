
==============================
Programming Notes
==============================

.. contents::
   :depth: 2

.. _sqlnull:

+++++++++++++++++++++
SQL NULL Values
+++++++++++++++++++++

SQL allows columns other than key columns to be NULL by default. Consider a YottaDB global node :code:`^USAddress("White House")="1600 Pennsylvania Ave NW||Washingtion|DC|20500-0005"` mapped to a table defined as follows:

.. code-block:: SQL

   CREATE TABLE USFamousAddresses(
     CommonName VARCHAR PRIMARY KEY,
     AddressLine1 VARCHAR,
     AddressLine2 VARCHAR,
     City VARCHAR,
     Territory VARCHAR(2),
     Zip VARCHAR(10)
   )
   GLOBAL "^USAddresses(keys(""CommonName""))";

The second piece of the node is an empty string (:code:`""`). There is no way to store a NULL as the piece of a YottaDB global variable node. Octo can either report that empty string as an empty string, or a NULL. The default is to treat it as a NULL. To treat it as a value, the column would be described in the DDL as NOT NULL:

.. code-block:: SQL

   CREATE TABLE USFamousAddresses(
     CommonName VARCHAR PRIMARY KEY,
     AddressLine1 VARCHAR,
     AddressLine2 VARCHAR NOT NULL,
     City VARCHAR,
     Territory VARCHAR(2),
     Zip VARCHAR(10)
   )
   GLOBAL "^USAddresses(keys(""CommonName""))";

When Octo encounters an empty string as the piece of a node (or the entire node) mapped to a column, the column is considered to have a value determined by the column type in the DDL, as follows:

+-----------------+-----------------------------------+-------------------------------+
| Column Type     | NOT NULL specified in DDL         | NOT NULL not specified in DDL |
+=================+===================================+===============================+
| INTEGER/NUMERIC | Treat empty value as 0            | Treat empty value as NULL     |
+-----------------+-----------------------------------+-------------------------------+
| VARCHAR/TEXT    | Treat empty value as empty string | Treat empty value as NULL     |
+-----------------+-----------------------------------+-------------------------------+
| BOOLEAN         | Treat empty value as FALSE        | Treat empty value as NULL     |
+-----------------+-----------------------------------+-------------------------------+

As described in the `Mapping to existing YottaDB global variables <./grammar.html#mapexisting>`_ Octo allows a 7-bit ASCII character to be designated as mapping to a SQL NULL. For such tables, an empty string as a piece is always treated as a value, as described in the table above.
