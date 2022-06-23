.. #################################################################
.. #								   #
.. # Copyright (c) 2018-2022 YottaDB LLC and/or its subsidiaries.  #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

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

Octo does not yet differentiate between these data types. All these types are currently treated as :code:`VARCHAR`. They can be used to store strings and can be followed by an optional size which specifies the maximum character length (not the byte length which could be different in case of non-ascii characters) of a string that can be stored in this column. Example: :code:`VARCHAR(20)` allows strings up to `20` characters to be stored.

As required by the SQL standard, an attempt to store a longer string into a column of these types will result in a :code:`VARCHAR_TOO_LONG` error, unless the excess characters are all spaces, in which case the string will be truncated to the maximum length.

If the string to be stored is shorter than the maximum column size, the shorter string will be stored as is.

As required by the SQL standard, if one explicitly casts a value to :code:`VARCHAR(n)`, then an over-length value will be truncated to :code:`n` characters without raising an error.

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

   SELECT *
   FROM names
   WHERE true;

.. note::

   Octo doesn't support :code:`t/f` like PostgreSQL does.


---------------
CREATE TABLE
---------------

.. code-block:: SQL

   CREATE TABLE table_name
   (column_name data_type [constraints][, ... column_name data_type [constraints]])
   [optional_keyword];

The CREATE TABLE statement is used to create tables in the database. The keywords CREATE TABLE are used followed by the name of the table to be created.

The names of columns to be created in the database and their datatypes are then specified in a list, along with any constraints that might need to apply (such as denoting a PRIMARY KEY, UNIQUE KEY, FOREIGN KEY or NOT NULL). If none of the columns are specified as keys (PRIMARY KEY or KEY NUM not specified in any column) then the primary key for the table is assumed to be the set of all columns in the order given.

Example:

.. code-block:: SQL

   CREATE TABLE Employee
   (ID int PRIMARY KEY,
   FirstName char(20),
   LastName char(30));

   CREATE TABLE Employee
   (ID int,
   FirstName char(20),
   LastName char(30));
   /* is equivalent to */
   CREATE TABLE Employee
   (ID int KEY NUM 0,
   FirstName char(20) KEY NUM 1,
   LastName char(30) KEY NUM 2);

By default, a column can have NULL values. The NOT NULL constraint enforces a column to **not** accept NULL values.

Example:

.. code-block:: SQL

   CREATE TABLE Employee
   (ID int PRIMARY KEY,
   FirstName char(20) NOT NULL,
   LastName char(30) NOT NULL);

The above example CREATEs a table named :code:`Employee`, where the :code:`FirstName` and :code:`LastName` columns cannot accept NULL values.

Note that CREATE TABLE statements can also accept a list of ASCII integer values for use in the DELIM qualifier, for example:

.. code-block:: SQL

   CREATE TABLE DELIMNAMES
   (id INTEGER PRIMARY KEY,
   firstName VARCHAR(30),
   lastName VARCHAR(30),
   middleInitial VARCHAR(1),
   age INTEGER)
   DELIM (9, 9) GLOBAL "^delimnames(keys(""id""))";

Here, two TAB characters (ASCII value 9) act as the internal delimiter of an Octo table. Note, however, that these delimiters are not applied to Octo output, which retains the default pipe :code:`|` delimiter. The reason for this is that tables may be joined that have different delimiters, so one common delimiter needs to be chosen anyway. Thus, the default is used.

If IF NOT EXISTS is supplied for a CREATE TABLE statement and a table exists, the result is a no-op with no errors. In this case, error type INFO_TABLE_ALREADY_EXISTS is emitted at INFO log severity level.

.. _mapexisting:

+++++++++++++++++++++++++++++++++++++++++++++
Mapping to existing YottaDB global variables
+++++++++++++++++++++++++++++++++++++++++++++

If mapping to existing YottaDB global variables, an optional_keyword can be added to further enhance the CREATE statement:

.. code-block:: none

   [ AIMTYPE | DELIM | END | ENDPOINT | EXTRACT | GLOBAL | KEY NUM | PIECE | READONLY | READWRITE | START | STARTINCLUDE ]

The keywords denoted above are M expressions and literals. They are explained in the following table:

+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| Keyword      | Type               | Range         | Purpose                                                                        | Overrides                    | Default Value                                             |
+==============+====================+===============+================================================================================+==============================+===========================================================+
| AIMTYPE      | Integer Literal    | Table         | By default, when Octo uses a YottaDB component called AIM to created indexes,  |                              | :code:`0`                                                 |
|              |                    |               | it does not include NULL data in the index. This can only happen when the data |                              |                                                           |
|              |                    |               | stored by Octo is on multiple levels, such as VistA data. Specifying "1" for a |                              |                                                           |
|              |                    |               | table means that we want data that is stored on other levels but is not        |                              |                                                           |
|              |                    |               | currently present to be considered NULL. If you specify "1", the region housing|                              |                                                           |
|              |                    |               | the table MUST have NULL subscripts in globals disabled.                       |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| DELIM        | Literal            | Table, Column | Represents the delimiter string to be used in                                  | table/default DELIM setting  | :code:`"|"`                                               |
|              |                    |               | `$PIECE() <https://docs.yottadb.com/ProgrammersGuide/functions.html#piece>`_   |                              |                                                           |
|              |                    |               | when obtaining the value of a particular column from the global variable       |                              |                                                           |
|              |                    |               | node that stores one row of the SQL table.  When specified at the column       |                              |                                                           |
|              |                    |               | level, an empty delimiter string (:code:`DELIM ""`) is allowed. In this        |                              |                                                           |
|              |                    |               | case, the entire global variable node value is returned as the column value    |                              |                                                           |
|              |                    |               | (i.e. no :code:`$PIECE` is performed).                                         |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| END          | Boolean expression | Table         | A condition that is tested to see if the cursor has gone past the last record  | Not applicable               | :code:`""=keys(0)`                                        |
|              |                    |               | in the table. If the condition evaluates to TRUE then that is considered past  |                              |                                                           |
|              |                    |               | the last record in the table.                                                  |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| ENDPOINT     | Literal            | Column        | Include all records including this value but not any value after it.           | Not applicable               | :code:`""=keys(0)`                                        |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| EXTRACT      | Expression         | Column        | Gets data based on the M expression following the EXTRACT keyword.             | PIECE, GLOBAL                | Not applicable                                            |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| GLOBAL       | Literal            | Table, Column | Represents the "source" location for a table. It consists of a global name     | table/default GLOBAL setting | :code:`^%ydboctoD_$zysuffix(TABLENAME)(keys("COLNAME"))`  |
|              |                    |               | followed by an optional list of subscripts. One may refer to a key column in   |                              | where :code:`TABLENAME` is the table name and             |
|              |                    |               | the subscript by specifying :code:`keys("COLNAME")` where :code:`COLNAME`      |                              | :code:`COLNAME` is the name of the primary key column.    |
|              |                    |               | is the name of the key column. Note that in the case of a :code:`READONLY`     |                              | If more than one key column exists, they will form more   |
|              |                    |               | table, if no key columns are specified, all columns in the order specified     |                              | subscripts. For example, if :code:`KEYCOL` is a column    |
|              |                    |               | are automatically assumed to be key columns. In case of a :code:`READWRITE`    |                              | that is specified with a :code:`PRIMARY KEY` keyword and  |
|              |                    |               | table, if no key columns are specified, a hidden key column is created by Octo |                              | :code:`KEYCOL2` is an additional column specified with a  |
|              |                    |               | with the name :code:`%YO_KEYCOL`. See examples in this document for how you    |                              | :code:`KEY NUM 1` keyword, then the default value would   |
|              |                    |               | can construct the GLOBAL keyword. If the Table-level GLOBAL keyword specifies  |                              | be :code:`^%ydboctoD...(keys("KEYCOL"),keys("KEYCOL2"))`  |
|              |                    |               | a global name with no subscripts, Octo adds subscripts to it one for every     |                              |                                                           |
|              |                    |               | key column that is explicitly specified or automatically assumed/generated     |                              |                                                           |
|              |                    |               | but if the Column-level GLOBAL keyword specifies a global name with no         |                              |                                                           |
|              |                    |               | subscripts no such automatic subscript addition takes place.                   |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| KEY NUM      | Integer Literal    | Column        | Specifies an integer indicating this column as part of a composite key.        | Not applicable               | Not applicable                                            |
|              |                    |               | The :code:`PRIMARY KEY` column correponds to :code:`KEY NUM 0`.                |                              |                                                           |
|              |                    |               | The first key column is specified with a :code:`PRIMARY KEY` keyword.          |                              |                                                           |
|              |                    |               | All other key columns are specified with a :code:`KEY NUM` keyword             |                              |                                                           |
|              |                    |               | with an integer value starting at :code:`1` and incrementing by 1 for          |                              |                                                           |
|              |                    |               | every key column. Such a column is considered a key column and is part of      |                              |                                                           |
|              |                    |               | the subscript in the global variable node that represents a row of the table.  |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| PIECE        | Integer Literal    | Column        | Represents a piece number. Used to obtain the value of a column in a table     | default (column number,      | Not applicable                                            |
|              |                    |               | by extracting this piece number from the value of the global variable node     | starting at 1 for non-key    |                                                           |
|              |                    |               | specified by the :code:`GLOBAL` keyword at this column level or at the table   | columns)                     |                                                           |
|              |                    |               | level. The generated code does a                                               |                              |                                                           |
|              |                    |               | `$PIECE() <https://docs.yottadb.com/ProgrammersGuide/functions.html#piece>`_   |                              |                                                           |
|              |                    |               | on the value to obtain the value. See also :code:`DELIM` keyword for the       |                              |                                                           |
|              |                    |               | delimiter string that is used in the :code:`$PIECE`.                           |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| READONLY     | Not applicable     | Table         | Specifies that the table maps to an existing YottaDB global variable           | Not applicable               | :code:`tabletype` setting in :code:`octo.conf`            |
|              |                    |               | and allows use of various keywords like :code:`START`, :code:`END` etc.        |                              |                                                           |
|              |                    |               | in the same :code:`CREATE TABLE` command. Queries that update tables like      |                              |                                                           |
|              |                    |               | :code:`INSERT INTO`, :code:`DELETE FROM` etc. are not allowed in such tables.  |                              |                                                           |
|              |                    |               | :code:`DROP TABLE` command drops the table and leaves the underlying mapping   |                              |                                                           |
|              |                    |               | global variable nodes untouched.                                               |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| READWRITE    | Not applicable     | Table         | Is the opposite of the :code:`READONLY` keyword. This allows queries that      | Not applicable               | :code:`tabletype` setting in :code:`octo.conf`            |
|              |                    |               | update tables like :code:`INSERT INTO`, :code:`DELETE FROM` etc. but does not  |                              |                                                           |
|              |                    |               | allow certain keywords like :code:`START`, :code:`END` etc. in the same        |                              |                                                           |
|              |                    |               | :code:`CREATE TABLE` command. That is, it does not allow a lot of flexibility  |                              |                                                           |
|              |                    |               | in mapping like :code:`READONLY` tables do. But queries that update tables     |                              |                                                           |
|              |                    |               | like :code:`INSERT INTO`, :code:`DELETE FROM` etc. are allowed in such tables. |                              |                                                           |
|              |                    |               | And a :code:`DROP TABLE` command on a :code:`READWRITE` table drops the table  |                              |                                                           |
|              |                    |               | and deletes/kills the underlying mapping global variable nodes.                |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| START        | Command expression | Column        | Indicates where to start a FOR loop (using                                     | Not applicable               | :code:`""`                                                |
|              |                    |               | `$ORDER() <https://docs.yottadb.com/ProgrammersGuide/functions.html#order>`_)  |                              |                                                           |
|              |                    |               | for a given key column in the table.                                           |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+
| STARTINCLUDE | Not applicable     | Column        | If specified, the FOR loop (using $ORDER()) that is generated for every key    | Not applicable               | Not specified                                             |
|              |                    |               | column in the physical plan processes includes the START value of the key      |                              |                                                           |
|              |                    |               | column as the first iteration of the loop. If not specified (the default),     |                              |                                                           |
|              |                    |               | the loop does a $ORDER() of the START value and uses that for the first        |                              |                                                           |
|              |                    |               | loop iteration.                                                                |                              |                                                           |
+--------------+--------------------+---------------+--------------------------------------------------------------------------------+------------------------------+-----------------------------------------------------------+

In the table above:

* table_name and cursor_name are variables representing the names of the table and the cursor being used.
* keys is a special variable in Octo that contains all of the columns that are identified as keys in the DDL (either via the "PRIMARY KEY" or "KEY NUM X" set of keywords).

If the same :code:`CREATE TABLE` command specifies :code:`READONLY` and :code:`READWRITE`, the keyword that is specified last (in left to right order of parsing the command) prevails. If neither of these two options are specified and :code:`octo.conf` does not specify :code:`tabletype = "READONLY"`, the table will be implicitly assumed to be :code:`READWRITE`.

A table will become :code:`READONLY` under the following conditions:

* If END, ENDPOINT, EXTRACT, SOURCE, START, or STARTINCLUDE keywords are used in the CREATE statement
* If the DELIM keyword is specified in the first non-key column and has a value other than :code:`""`
* If the PIECE number is not the same as the column number (first column is 1, second column is 2, etc.)
* If the GLOBAL keyword is specified with subscripts that are not in a format compatible with READWRITE

If a :code:`DELIM ""` is specified for a column, any :code:`PIECE` keyword specified for that column is ignored and is treated as if the keyword was not specified.

For :code:`ENDPOINT`, you can specify literals, M style $CHAR data, or a space. Note that to specify a space, you need to say :code:`'" "'`. For $CHAR(n), say :code:`'$CHAR(n)'`. Note that if you specify an empty string (:code:`'""'`), you will get no records. In this case you should just omit :code:`ENDPOINT`.

You can combine :code:`END` and :code:`ENDPOINT` together. If you do so, both conditions are evaluated; however, the END condition is evaluated before the ENDPOINT condition.

~~~~~~~~~~~
Examples
~~~~~~~~~~~

.. code-block:: SQL

   CREATE TABLE Orders
   (OrderID INTEGER PRIMARY KEY,
    CustomerID INTEGER,
    EmployeeID INTEGER,
    OrderDate VARCHAR(16),
    ShipperID INTEGER)
   GLOBAL "^Orders(keys(""OrderID""))";

In the above example, the :code:`Orders` table maps data in the nodes of the global variable :code:`^Orders`. :code:`^Orders` has a single subscript, :code:`OrderID`. Its nodes are strings, whose :code:`|` separated pieces are, respectively, :code:`CustomerID`, :code:`EmployeeID`, :code:`OrderDate`, and :code:`ShipperID`, e.g., :code:`^Orders(535088)="9015|57|2021-08-26|17"`. :code:`"|"` is the default piece operator.

.. code-block:: SQL

   CREATE TABLE Orders
   (OrderID INTEGER PRIMARY KEY,
    CustomerID INTEGER,
    EmployeeID INTEGER,
    OrderDate VARCHAR(16),
    ShipperID INTEGER)
   DELIM "^"
   GLOBAL "^Orders(keys(""OrderID""))";

This example is similar to the last, except that the nodes of :code:`^Orders` are strings whose pieces are separated by :code:`"^"`, e.g., :code:`^Orders(535088)="9015^57^2021-08-26^17"`.

.. code-block:: SQL

   CREATE TABLE USPresidents
   (FirstYear INTEGER PRIMARY KEY,
    LastYear INTEGER KEY NUM 1,
    FirstName VARCHAR,
    MiddleName VARCHAR,
    LastName VARCHAR,
    BirthYear INTEGER,
    DeathYear INTEGER)
   GLOBAL "^USPresidents(keys(""FirstYear""),keys(""LastYear""))";

In the above example, ^USPresidents has records like :code:`^USPresidents(1933,1945)="Franklin|Delano|Roosevelt|1882|1945"` and :code:`^USPresidents(2009,2017)="Barack||Obama|1961"`.

.. code-block:: SQL

   CREATE TABLE PresidentNames
   (ID INTEGER PRIMARY KEY,
    FName VARCHAR PIECE 2,
    LName VARCHAR PIECE 1)
   GLOBAL "^PresidentNames(keys(""ID""))";

In the above example, ^PresidentNames has records like :code:`^Names(1)="Lincoln|Abraham"` and :code:`^Names(2)="Obama|Barack"`.

.. code-block:: SQL

   CREATE TABLE AuthorNames
   (ID INTEGER PRIMARY KEY,
    LName VARCHAR ,
    FName VARCHAR EXTRACT "$PIECE(^AuthorNames(keys(""ID"")),""^"",2)")
   DELIM "^"
   GLOBAL "^AuthorNames(keys(""ID""))";

In the above example, ^AuthorNames has records like :code:`^Names(1)="Dahl^Roald"` and :code:`^Names(2)="Blyton^Enid"`.

.. code-block:: SQL

   CREATE TABLE Orders
   (OrderID INTEGER PRIMARY KEY,
    CustomerID INTEGER,
    EmployeeID INTEGER,
    OrderDate VARCHAR(16),
    ShipperID INTEGER)
   GLOBAL "^Orders(keys(""OrderID""))"
   READONLY;

In the above example, the :code:`Orders` table is set to be :code:`READONLY`. If the :code:`Orders` table is DROPped then the underlying mapped global variable node (:code:`^Orders`) will be untouched.

.. code-block:: SQL

   CREATE TABLE Orders
   (OrderID INTEGER PRIMARY KEY,
    CustomerID INTEGER,
    EmployeeID INTEGER,
    OrderDate VARCHAR(16),
    ShipperID INTEGER)
   GLOBAL "^Orders(keys(""OrderID""))"
   READWRITE;

In the above example, the :code:`Orders` table is set to be :code:`READWRITE`. If the :code:`Orders` table is DROPped then the underlying mapped global variable nodes (:code:`^Orders`) will be deleted.

.. code-block:: SQL

   CREATE TABLE Orders
   (OrderID INTEGER PRIMARY KEY START 0 END "$CHAR(0)]]keys(""OrderID"")",
    CustomerID INTEGER,
    EmployeeID INTEGER,
    OrderDate VARCHAR(16),
    ShipperID INTEGER)
   GLOBAL "^Orders(keys(""OrderID""))";

In the above example, the START and END keywords tell Octo what subset of the ^Orders nodes with one subscript should be mapped to the Orders table. :code:`START 0` indicates that subscripts greater than :code:`0` should be mapped, and :code:`END "$CHAR(0)]]keys(""OrderID"")"` restricts the mapping to numeric subscripts.

Rather than using END in the previous example, you can use the simpler ENDPOINT, which will achieve the same result (the below example illustrates that). ENDPOINT will traverse the global until it reaches the specified endpoint, and it will include the end point record as well. Most of the time, ENDPOINT should be used to reach the end of a numeric subscript range. Therefore, a good value to use is :code:`'$CHAR(0)'` or :code:`'" "'`, as these sort after numbers.

.. code-block:: SQL

   CREATE TABLE Orders
   (OrderID INTEGER PRIMARY KEY START 0 ENDPOINT '$CHAR(0)',
    CustomerID INTEGER,
    EmployeeID INTEGER,
    OrderDate VARCHAR(16),
    ShipperID INTEGER)
   GLOBAL "^Orders(keys(""OrderID""))";


.. code-block:: SQL

   CREATE TABLE Orders
   (OrderID INTEGER PRIMARY KEY START 1 END "'+keys(""OrderID"")" STARTINCLUDE,
    CustomerID INTEGER,
    EmployeeID INTEGER,
    OrderDate VARCHAR(16),
    ShipperID INTEGER)
   GLOBAL "^Orders(keys(""OrderID""))";

In the above example STARTINCLUDE is used with START and END. In this case the FOR loop for `$ORDER() <https://docs.yottadb.com/ProgrammersGuide/functions.html#order>`_ includes the START value of the key column as the first iteration of the loop.

+++++++++++++
Error Case
+++++++++++++

.. note::
   A CREATE TABLE waits for all other concurrently running queries(SELECT or CREATE TABLE or DROP TABLE) to finish so it can safely make DDL changes. It waits for an exclusive lock with a timeout of 10 seconds. If it fails due to a timeout, the user needs to stop all concurrently running queries and reattempt the CREATE TABLE statement.

---------------
CREATE FUNCTION
---------------

.. code-block:: SQL

   CREATE FUNCTION function_name
   ([data_type[, data_type[, ...]]])
   RETURNS data_type AS extrinsic_function_name;

The CREATE FUNCTION statement is used to create SQL functions that map to extrinsic M functions and store these mappings in the database. The keywords CREATE FUNCTION are followed by the name of the SQL function to be created, the data types of its parameters, its return type, and the fully-qualified extrinsic M function name.

Note that Octo reserves the M routine prefix :code:`^%ydbocto` for internal functions defined by Octo itself. Moreover, Octo assumes that any YottaDB extrinsic function name that includes this prefix but omits a label will have its own :code:`_ydbocto*.m` file containing emulation label mappings for :code:`PostgreSQL` and :code:`MySQL`. Accordingly, extrinsic function names like `$$^ydboctoxyz` will prompt Octo to look for a :code:`_ydboctoxyz.m` file containing two labels, :code:`PostgreSQL` and :code:`MySQL`. If these labels are absent, a `LABELMISSING` will be issued by YottaDB. For this reason, it is advised that users do not use the :code:`^%ydbocto` prefix in extrinsic function names to avoid conflicts and complications with Octo internal M routines.

CREATE FUNCTION can be used to define multiple functions with the same name, provided the number of parameters and/or the types of the parameters are different. In other words, CREATE FUNCTION supports function overloading.

However, functions cannot be overloaded based on their return type. For example, if two CREATE FUNCTION calls are made with the same name and parameter types, but a different return type, the return type of the last executed statement will be retained and the first discarded. Accordingly, care should be used when overloading functions, particularly when specifying varied return types for a single function.

The SQL function's parameter data types are specified in a list, while the data type of the return value must be a single value (only one object can be returned from a function). The extrinsic function name must be of the form detailed in the `M Programmer's Guide <https://docs.yottadb.com/ProgrammersGuide/langfeat.html#id8>`__.

When a function is created from a CREATE FUNCTION statement, an entry is added to Octo's internal PostgreSQL catalog. In other words, a row is added to the :code:`pg_catalog.pg_proc` system table. To view a list of created functions, their argument number and type(s), and return argument type, you can run:

.. code-block:: SQL

   select proname,pronargs,prorettype,proargtypes
   from pg_proc;

Type information for each function parameter and return type will be returned as an OID. This OID can be used to look up type information, including type name, from the :code:`pg_catalog.pg_type` system table. For example, to retrieve the human-readable return type and function name of all existing functions:

.. code-block:: SQL

   select proname,typname
   from pg_catalog.pg_proc
   inner join pg_catalog.pg_type on pg_catalog.pg_proc.prorettype = pg_catalog.pg_type.oid;

However, function parameter types are currently stored as a list in a VARCHAR string, rather than in a SQL array as the latter isn't yet supported by Octo. In the meantime, users can lookup the type name corresponding to a given type OID by using the following query:

.. code-block:: SQL

   select oid,typname
   from pg_catalog.pg_type;

Note that CREATE FUNCTION is the preferred method for creating new SQL functions and manually creating these functions through direct database modifications is not advised.

Example:

.. code-block:: none

   CREATE FUNCTION ADD(int, int)
   RETURNS int AS $$ADD^myextrinsicfunction;

   CREATE FUNCTION APPEND(varchar, varchar)
   RETURNS varchar AS $$APPEND;

To create a parameterless function, the parameter type list may be omitted by leaving the parentheses blank:

Example:

.. code-block:: none

   CREATE FUNCTION userfunc()
   RETURNS int AS $$userfunc^myextrinsicfunction;

If IF NOT EXISTS is supplied for a CREATE FUNCTION statement and a function exists, the result is a no-op with no errors. In this case, error type INFO_FUNCTION_ALREADY_EXISTS is emitted at INFO log severity level.

+++++++++++++
Error Case
+++++++++++++

.. note::
   A CREATE FUNCTION waits for all other concurrently running queries(SELECT or CREATE TABLE or DROP TABLE) to finish so it can safely make DDL changes. It waits for an exclusive lock with a timeout of 10 seconds. If it fails due to a timeout, the user needs to stop all concurrently running queries and reattempt the CREATE FUNCTION statement.

---------------
DISCARD ALL
---------------

.. code-block:: SQL

   DISCARD ALL;

As needed, Octo automatically creates physical plans, cross references, database triggers, and other internal artifacts that allow it to execute queries correctly and quickly. The DISCARD ALL command deletes these internal artifacts. Octo also automatically discards artifacts when appropriate, for example when the schema changes or after Octo upgrades.

The DISCARD ALL command is safe to run at any time. As running a DISCARD command will cause subsequent commands to run slowly as Octo recreates required artifacts, use it when you need to minimize the size of an Octo environment, for example, to distribute it or archive it.

-----------------
DROP TABLE
-----------------

.. code-block:: SQL

   DROP TABLE [IF EXISTS] table_name [KEEPDATA];

The DROP TABLE statement is used to remove tables from the database. The keywords DROP TABLE are followed by the name of the table desired to be dropped.

.. Optional parameters include CASCADE and RESTRICT.
.. The CASCADE parameter is used to specify that all objects depending on the table will also be dropped.
.. The RESTRICT parameter is used to specify that the table referred to by table_name will not be dropped if there are existing objects depending on it.

Example:

.. code-block:: SQL

   DROP TABLE Employee;

If :code:`IF EXISTS` is supplied for a :code:`DROP TABLE` statement and a table does not exist, the result is a no-op with no errors. In this case, error type :code:`INFO_TABLE_DOES_NOT_EXIST` is emitted at :code:`INFO` log severity level.

By default, a :code:`DROP TABLE` statement for a :code:`READWRITE` table drops the table and also kills all underlying global nodes that stored the table data. The optional parameter :code:`KEEPDATA` overrides this behavior, preserving the underlying global nodes regardless of table writability type. :code:`DROP TABLE` statements for :code:`READONLY` tables always preserve the underlying global nodes whether :code:`KEEPDATA` is explicitly specified or not.

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

If IF EXISTS is supplied for a DROP FUNCTION statement and a function does not exist, the result is a no-op with no errors. In this case, error type INFO_FUNCTION_DOES_NOT_EXIST is emitted at INFO log severity level.

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

+++++++++++++++++
SELECT *
+++++++++++++++++

:code:`SELECT *` is used as a shorthand for all the columns of the selected rows to be part of the output list. :code:`SELECT table_name.*` is used as a shorthand for the columns coming from just the table **table_name**. All the columns in the table **table_name** are considered for processing in the order they appear.

++++++
FROM
++++++

This clause specifies the table(s) from which the columns are selected.

**from_item** can be any of the following:

    - **table_name** : The name of an existing table.

        .. code-block:: SQL

	   /* Selects all rows from the table names */
	   SELECT *
	   FROM names;

    - **alias** : A temporary name given to a table or a column for the purposes of a query. Please refer the :ref:`sql-alias` section below for more information.

        .. code-block:: SQL

	   /* Selects all rows from the table names aliased as n */
	   SELECT *
	   FROM names AS n;

    - **select** : A SELECT subquery, which must be surrounded by parentheses. Examples showcasing the usage of the SELECT subquery can be found in the :ref:`sql-table-alias` section below.


    - **join_type** : Any one of the :ref:`sql-joins`. A **join_type** cannot be the first **from_item**. Examples showcasing the usage of **join_type** can be found in the :ref:`sql-joins` section below.

.. _sql-joins:

~~~~~~~
JOINS
~~~~~~~

Joins can be made by appending a join type and table name to a SELECT statement:

.. code-block:: SQL

   [CROSS | [NATURAL | INNER | [LEFT][RIGHT][FULL] OUTER]] JOIN ON joined_table;

A **CROSS JOIN** between two tables provides the number of rows in the first table multiplied by the number of rows in the second table.

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

   /* Selects the first name, last name and address of an employee that have an address. The employee and address table are joined on the employee ID values. */
   SELECT FirstName, LastName, Address
   FROM Employee
   INNER JOIN Addresses ON Employee.ID = Addresses.EID;

.. note::

   Currently only the INNER and OUTER JOINs support the ON clause.

++++++++
WHERE
++++++++

This clause represents a condition under which columns are selected. If the **search_condition** evaluates to true, that row is part of the output otherwise it is excluded.

+++++++++++
GROUP BY
+++++++++++

The GROUP BY clause provides for result rows to be grouped together based on the specified **grouping_column**. **grouping_column** can be :code:`table_name.*` or SELECT list column number or an expression. In case of :code:`table_name.*` all columns of the table are considered for processing.

++++++++++
HAVING
++++++++++

The HAVING clause works to filter the rows that result from the GROUP BY clause. The rows are filtered based on the boolean value returned by the **search_condition**.

See :ref:`technical-notes` for details on value expressions.

Example:

.. code-block:: SQL

   /* Selects the Employee ID, first name and last name from the employee table for employees with ID greater than 100. The results are grouped by the last name of the employees. */
   SELECT ID, FirstName, LastName
   FROM Employee
   WHERE ID > 100
   GROUP BY LastName;

++++++++++
ORDER BY
++++++++++

ORDER BY lets you sort the order of the rows returned after the query.

To sort rows or columns in the database, you need to have one of the following **sort_specifications**.

.. code-block:: SQL

   sort_key [COLLATE collation_name] [ASC | DESC];

The **sort_key** can be a :code:`column reference`, :code:`literal` or the shorthand :code:`table_name.*`.

The sort key can be followed by a collate clause, ordering specification or both.

.. note::
   A collation is a set of rules to compare characters in a character set.

The collate clause consists of the word COLLATE and the relevant collation name.

The ordering specification lets you further choose whether to order the returned columns in ascending (ASC) or descending (DESC) order.

Example:

.. code-block:: SQL

   /* Selects the Employee ID, first name and last name from the employee table for employees with ID greater than 100. The results are ordered in descending order of ID. */
   SELECT ID, FirstName, LastName
   FROM Employee
   WHERE ID > 100
   ORDER BY ID DESC;

+++++++
LIMIT
+++++++

This clause allows the user to specify the number of rows they want to retrieve from the results of the query.

Example:

.. code-block:: SQL

   /* Selects the first five rows from the employee table */
   SELECT *
   FROM Employee
   LIMIT 5;

The above example returns no more than five rows.

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

.. code-block:: SQL

   INSERT INTO table_name ( column name [, column name ...]) [ VALUES ... | (SELECT ...)];

The INSERT statement allows you to insert values into a table. These can either be provided values or values specified as a result of a SELECT statement.

Example:

.. code-block:: SQL

   INSERT INTO Employee (ID , FirstName, LastName) VALUES (220, 'Jon', 'Doe'), (383, 'Another', 'Name');

--------------
UPDATE
--------------

.. code-block:: SQL

   UPDATE table_name [[AS] alias_name] SET column1 = expression [, column2 = expression ...] [WHERE search_condition];

:code:`table_name` specifies the name of the table to be updated followed by a list of comma-separated statements that are used to update existing columns in the table with specified values. Only those columns in :code:`table_name` that require change need to be mentioned in the :code:`SET` clause. The remaining columns retain their previous values. The optional WHERE condition allows you to update columns only on those rows of the table that satisfy the specified :code:`search_condition`.

Example:

.. code-block:: SQL

   UPDATE Employee SET FirstName = 'John' WHERE ID = 220;

------------
DELETE
------------

.. code-block:: SQL

   DELETE FROM table_name [[AS] alias_name] [WHERE search_condition];

The DELETE statement consists of the keywords DELETE FROM followed by the name of the table and possibly a search condition.

The search condition eventually yields a boolean true or false value, and may contain further search modifications detailing where to apply the search_condition and how to compare the resulting values.

Example:

.. code-block:: SQL

   DELETE FROM Employee WHERE ID = 220;

--------------
SET
--------------

*(Partially supported.)*

.. code-block:: SQL

   SET runtime_parameter = value;

The SET command changes the value of a run-time configuration parameter. Presently, Octo does not honor such parameter settings itself, but merely provides the SET interface for compatibility with PostgreSQL clients. Note that run-time parameter names are case-insensitive when using SET.

Example:

.. code-block:: SQL

   SET DateStyle = 'ISO';

Runtime parameter information is maintained in the :code:`pg_catalog.pg_settings` PostgreSQL catalog table. Using a SET command to change the value of a run-time parameter will also update the entry for that parameter in :code:`pg_catalog.pg_settings`.

Note that SET commands treat SQL NULL values as empty strings. For example, the following command sets the DateStyle parameter to the empty string:

.. code-block:: SQL

    SET DateStyle = NULL;

Note that updates to :code:`pg_catalog.pg_settings` using the :code:`INSERT INTO`, :code:`DELETE FROM` or :code:`UPDATE` commands are disallowed (would issue a :code:`ERR_TABLE_READONLY` error).

--------------
SHOW
--------------

.. code-block:: SQL

   SHOW runtime_parameter;

The SHOW command prints the value of a run-time configuration parameter. Note that run-time parameter names are case-insensitive when using SHOW.

Example:

.. code-block:: SQL

   SHOW DateStyle;

Runtime parameter information is maintained in the :code:`pg_catalog.pg_settings` PostgreSQL catalog table. Accordingly, run-time parameter information may be viewed by querying this table. When using this method, the parameter name is case-sensitive, as the name must will be looked up by comparing the given literal value against a canonical name in the database.

Example:

.. code-block:: SQL

   SELECT name, setting FROM pg_catalog.pg_settings WHERE name = 'DateStyle';

To list of all run-time parameter information:

.. code-block:: SQL

   SELECT * FROM pg_catalog.pg_settings;

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

   SELECT [.....]
   FROM table_name[...]
   UNION
   [ALL] SELECT [.....]
   FROM table_name2[...]....;

The UNION operation consists of two or more queries joined together with the word UNION.  It combines the results of two individual queries into a single set of results.

The keyword ALL ensures that duplicate rows of results are not removed during the UNION.

Example:

.. code-block:: SQL

   SELECT FirstName
   FROM Employee
   UNION
   SELECT FirstName
   FROM AddressBook;

++++++++++++++++
INTERSECT
++++++++++++++++

.. code-block:: SQL

   SELECT [.....]
   FROM table_name[......]
   INTERSECT
   [ALL] SELECT [.....]
   FROM table_name2[....]......;

The INTERSECT operation consists of two or more queries joined together with the word INTERSECT. It returns distinct non-duplicate results that are returned by both queries on either side of the operation.

The keyword ALL ensures that duplicate rows of results returned by both queries are not eliminated during the INTERSECT.

.. code-block:: SQL

   SELECT ID
   FROM Employee
   INTERSECT
   SELECT ID
   FROM AddressBook;

++++++++++++++
EXCEPT
++++++++++++++

.. code-block:: SQL

   SELECT [.....]
   FROM table_name[.....]
   EXCEPT
   [ALL] SELECT [.....]
   FROM table_name2[......].......;

The EXCEPT operation consists of two or more queries joined together with the word EXCEPT. It returns (non-duplicate) results from the query on the left side except those that are also part of the results from the query on the right side.

The keyword ALL affects the resulting rows such that duplicate results are allowed but rows in the first table are eliminated if there is a corresponding row in the second table.

.. code-block:: SQL

   SELECT LastName
   FROM Employee
   EXCEPT
   SELECT LastName
   FROM AddressBook;

--------------
VALUES
--------------

:code:`VALUES` provides a way to generate an "on-the-fly" table that can be used in a query without having to actually create and populate a table on-disk.

The syntax is:

.. code-block:: SQL

   VALUES ( expression [, ...] ) [, ...]

Each parenthesized list of expressions generates one row in the table. Each specified row must have the same number of comma-separated entries (could be constants, expressions, subqueries etc.). This becomes the number of columns in the generated table. Corresponding entries in each row must have compatible data types. The data type assigned to each column of the generated table is determined based on the data type of the entries in the row lists.

The columns of the generated table are assigned the names :code:`column1`, :code:`column2`, etc.

For example, the below generates a table of two columns and three rows.

.. code-block:: SQL

   VALUES (1, 'one'), (2, 'two'), (3, 'three');

will return a table containing two columns (named :code:`column1` with type INTEGER and :code:`column2` with type VARCHAR) and three rows.

:code:`VALUES` followed by expression lists can appear anywhere a :code:`SELECT` can.  So, the below two queries are equivalent.

.. code-block:: SQL

   VALUES (1, 'one'), (2, 'two'), (3, 'three');
   SELECT 1, 'one' UNION SELECT 2, 'two' UNION SELECT 3, 'three';

There is an exception to this currently in that :code:`ORDER BY` and :code:`LIMIT` cannot be specified at the end of :code:`VALUES` like they can be for :code:`SELECT`.

Below are examples of using :code:`VALUES` with entries containing expressions and subqueries:

.. code-block:: SQL

   SELECT 5 + (VALUES (3));
   SELECT * FROM (VALUES ((SELECT 1), 2));
   VALUES((SELECT id FROM names WHERE id > 5));

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

----------
Functions
----------

Octo supports the following built-in functions. Each of these functions comes pre-defined with Octo, and can be used straightaway without the need for the user to define them.

Note that function prototypes that appear both with and without parentheses indicate that the given function may be called both with and without parentheses. For example, :code:`CURRENT_CATALOG()` may be called with either :code:`CURRENT_CATALOG()` or :code:`CURRENT_CATALOG`.

+++++
ABS
+++++

.. code-block:: SQL

   ABS(NUMERIC)

ABS returns the absolute value of a number.

++++++++++
COALESCE
++++++++++

.. code-block:: SQL

   COALESCE(value_expression [, value_expression...])

The built-in COALESCE function returns the first of its arguments that is not NULL.
If all arguments are NULL, NULL is returned.
COALESCE must have at least one argument.

The arguments passed to COALESCE all have to be of the same type.
For example, the following query is valid and returns the value 'a':

.. code-block:: SQL

   SELECT COALESCE(NULL, 'a', 'b');

++++++++++
CONCAT
++++++++++

.. code-block:: SQL

   CONCAT(VARCHAR, VARCHAR)
   CONCAT(VARCHAR, VARCHAR, VARCHAR)

The built-in CONCAT function returns the concatenation of its arguments as a VARCHAR value. This function may be used with two or three VARCHAR arguments to be concatenated.

.. code-block:: SQL

   SELECT CONCAT('string1', 'string2')
   SELECT CONCAT('string1', 'string2', 'string3')

+++++++++++++++
CURRENT_CATALOG
+++++++++++++++

.. code-block:: SQL

   CURRENT_CATALOG
   CURRENT_CATALOG()

The built-in CURRENT_CATALOG function returns the name of the current database catalog. However, since Octo currently does not support the use of more than one database catalog, this function always returns "octo".

++++++++++++++++
CURRENT_DATABASE
++++++++++++++++

.. code-block:: SQL

   CURRENT_DATABASE()

The built-in CURRENT_DATABASE function returns the name of the current database. However, since Octo currently does not support the use of more than one database, this function always returns "octo".

++++++++++++
CURRENT_ROLE
++++++++++++

.. code-block:: SQL

   CURRENT_ROLE
   CURRENT_ROLE()

The built-in CURRENT_ROLE function returns the name of the current user role. However, since Octo currently does not support user roles, this function is an alias for CURRENT_USER().

++++++++++++++
CURRENT_SCHEMA
++++++++++++++

.. code-block:: SQL

   CURRENT_SCHEMA
   CURRENT_SCHEMA()

The built-in CURRENT_SCHEMA function returns the name of the current database schema. However, since Octo currently does not multiple schemas, this function will always return "public".

++++++++++++
CURRENT_TIME
++++++++++++

.. code-block:: SQL

   CURRENT_TIME
   CURRENT_TIME()

The built-in CURRENT_TIME returns the current system time in the following formats, depending on which database emulation setting is active:

* :code:`POSTGRES` emulation: :code:`hh:mm:ss.UUUUUU[-|+]LL`, where `U` is a microsecond field and `[-|+]LL` is the positive or negative UTC offset.
* :code:`MYSQL` emulation: :code:`hh:mm:ss`

+++++++++++++++++
CURRENT_TIMESTAMP
+++++++++++++++++

.. code-block:: SQL

   CURRENT_TIMESTAMP
   CURRENT_TIMESTAMP()

The built-in CURRENT_TIMESTAMP is a synonym for the NOW function, and returns the current system time in the following formats, depending on which database emulation setting is active:

* :code:`POSTGRES` emulation: :code:`YYYY-MM-DD hh:mm:ss.uuuuuu[-|+]LL`, where `u` is a microsecond field and `[-|+]LL` is the positive or negative UTC offset.
* :code:`MYSQL` emulation: :code:`YYYY-MM-DD hh:mm:ss`

++++++++++++
CURRENT_USER
++++++++++++

.. code-block:: SQL

   CURRENT_USER
   CURRENT_USER()

The built-in CURRENT_USER function returns the username of the current Rocto user. Does not work in Octo, since Octo does not implement SQL user authentication and does not distinguish between users.

+++
DAY
+++

.. code-block:: SQL

   DAY(VARCHAR)

The built-in DAY function is a synonym for DAYOFMONTH, and accepts a date in the format :code:`YYYY-MM-DD` and returns the numeric day of the month in the range 0-31 for dates that have a value of zero for the day field, e.g. `0000-00-00`.

++++++++++
DAYOFMONTH
++++++++++

.. code-block:: SQL

   DAYOFMONTH(VARCHAR)

The built-in DAYOFMONTH function accepts a date in the format :code:`YYYY-MM-DD` and returns the numeric day of the month in the range 0-31 for dates that have a value of zero for the day field, e.g. `1999-06-00`.

+++++++++++
DATE_FORMAT
+++++++++++

.. code-block:: SQL

   DATE_FORMAT(VARCHAR)

The built-in DATE_FORMAT function accepts a date in the format :code:`YYYY-MM-DD hh:mm:ss.uuuuuu` and a format string, and returns a new string wherein the given date is formatted according to the format specified. Note that the number of microseconds :code:`uuuuuu` may be omitted such that the date may be in the format :code:`YYYY-MM-DD hh:mm:ss`.

Note that in the following table there is reference to MySQL :code:`WEEK()` modes. Presently, Octo does not implement `WEEK()`, but the MySQL `WEEK()` modes are implemented for those format codes below that require them. For more information on :code:`WEEK()` modes, see the `MySQL documentation <https://dev.mysql.com/doc/refman/8.0/en/date-and-time-functions.html#function_week>`_.

Acceptable formatting symbols for DATE_FORMAT format string are as follows:

+----------------+--------------------------------------------------------------------------------------------------------------+
| Format symbol  | Description                                                                                                  |
+================+==============================================================================================================+
| %a             | Abbreviated weekday name (Sun..Sat)                                                                          |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %b             | Abbreviated month name (Jan..Dec)                                                                            |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %c             | Month, numeric (0..12)                                                                                       |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %D             | Day of the month with English suffix (0th, 1st, 2nd, 3rd, ...)                                               |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %d             | Day of the month, numeric (00..31)                                                                           |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %e             | Day of the month, numeric (0..31)                                                                            |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %f             | Microseconds (000000..999999)                                                                                |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %H             | Hour (00..23)                                                                                                |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %h             | Hour (01..12)                                                                                                |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %I             | Hour (01..12)                                                                                                |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %i             | Minutes, numeric (00..59)                                                                                    |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %j             | Day of year (001..366)                                                                                       |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %k             | Hour (0..23)                                                                                                 |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %l             | Hour (1..12)                                                                                                 |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %M             | Month name (January..December)                                                                               |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %m             | Month, numeric (00..12)                                                                                      |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %p             | AM or PM                                                                                                     |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %r             | Time, 12-hour (hh:mm:ss followed by AM or PM)                                                                |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %S             | Seconds (00..59)                                                                                             |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %s             | Seconds (00..59)                                                                                             |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %T             | Time, 24-hour (hh:mm:ss)                                                                                     |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %U             | Week (00..53), where Sunday is the first day of the week; Corresponding to MySQL WEEK() mode 0               |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %u             | Week (00..53), where Monday is the first day of the week; Corresponding to MySQL WEEK() mode 1               |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %V             | Week (01..53), where Sunday is the first day of the week; Corresponding to MySQL WEEK() mode 2; used with %X |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %v             | Week (01..53), where Monday is the first day of the week; Corresponding to MySQL WEEK() mode 3; used with %x |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %W             | Weekday name (Sunday..Saturday)                                                                              |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %w             | Day of the week (0=Sunday..6=Saturday)                                                                       |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %X             | Year for the week where Sunday is the first day of the week, numeric, four digits; used with %V              |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %x             | Year for the week, where Monday is the first day of the week, numeric, four digits; used with %v             |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %Y             | Year, numeric, four digits                                                                                   |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %y             | Year, numeric (two digits)                                                                                   |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %%             | A literal % character                                                                                        |
+----------------+--------------------------------------------------------------------------------------------------------------+
| %x             | x, for any "x" not listed above                                                                              |
+----------------+--------------------------------------------------------------------------------------------------------------+

.. code-block:: SQL

    OCTO> SELECT DATE_FORMAT('2004-10-22 21:20:14', '%W %M %Y');
    DATE_FORMAT
    Friday October 2004
    (1 row)
    OCTO> SELECT DATE_FORMAT('2019-10-22 21:20:14', '%H:%i:%s');
    DATE_FORMAT
    21:20:14
    (1 row)
    OCTO> SELECT DATE_FORMAT('1920-10-22 21:20:14', '%D %y %a %d %m %b %j');
    DATE_FORMAT
    22nd 20 Fri 22 10 Oct 296
    (1 row)
    OCTO> SELECT DATE_FORMAT('1994-10-22 21:20:14', '%H %k %I %r %T %S %w');
    DATE_FORMAT
    21 21 09 09:20:14 PM 21:20:14 14 6
    (1 row)
    OCTO> SELECT DATE_FORMAT('1999-01-01', '%X %V');
    DATE_FORMAT
    1998 52
    (1 row)
    OCTO> SELECT DATE_FORMAT('2006-06-00', '%d');
    DATE_FORMAT
    00
    (1 row)

++++++++++++++++++
GREATEST and LEAST
++++++++++++++++++

.. code-block:: SQL

   GREATEST(value_expression [, value_expression...])
   LEAST(value_expression [, value_expression...])

The built-in GREATEST function returns the largest value from a list of expressions.
Similarly, LEAST returns the smallest value.
NULL values are ignored, unless all values are NULL, in which case the return value is NULL.
All arguments must have the same type.

++++++++++++++++++++++
HAS_DATABASE_PRIVILEGE
++++++++++++++++++++++

.. code-block:: SQL

    HAS_DATABASE_PRIVILEGE(username, databasename, privilege)

The built-in HAS_DATABASE_PRIVILEGE function returns true if the user (first argument) of the specified database (second argument) has the specified privilege (third argument). However, since Octo currently does not implement privileges, this function will always return true (1).

+++++++++
LOCALTIME
+++++++++

.. code-block:: SQL

   LOCALTIME
   LOCALTIME()

The built-in LOCALTIME function returns the current system time in the following formats, depending on which database emulation setting is active:

* :code:`POSTGRES` emulation: :code:`hh:mm:ss.UUUUUU[-|+]LL`, where `U` is a microsecond field and `[-|+]LL` is the positive or negative UTC offset.
* :code:`MYSQL` emulation (synonym for NOW): :code:`YYYY-MM-DD hh:mm:ss`

++++++++++++++
LOCALTIMESTAMP
++++++++++++++

.. code-block:: SQL

   LOCALTIMESTAMP
   LOCALTIMESTAMP()

The built-in LOCALTIMESTAMP is a synonym for the NOW function, and returns the current system time in the following formats, depending on which database emulation setting is active:

* :code:`POSTGRES` emulation: :code:`YYYY-MM-DD hh:mm:ss.UUUUUU[-|+]LL`, where `U` is a microsecond field and `[-|+]LL` is the positive or negative UTC offset.
* :code:`MYSQL` emulation: :code:`YYYY-MM-DD hh:mm:ss`

++++++++++++++++
LPAD
++++++++++++++++

.. code-block:: SQL

    LPAD(VARCHAR, INTEGER)
    LPAD(VARCHAR, INTEGER, VARCHAR)

The built-in LPAD function adds padding to the left hand side of a string (first argument) up to the designated length (second argument). The default padding is a space, which is used in the two-argument form of this function. However, an optional third argument specifying a specific string to use for padding may also be used.

Note that in :code:`POSTGRES` emulation either the two- or three- argument form may be used. However, MySQL only supports the three-argument version, so a third argument must always be specified when using the :code:`MYSQL` emulation setting.

++++++++
NOW
++++++++

.. code-block:: SQL

   NOW()

The built-in NOW function returns the current system time in the following formats, depending on which database emulation setting is active:

* :code:`POSTGRES` emulation: :code:`YYYY-MM-DD hh:mm:ss.UUUUUU[-|+]LL`, where `U` is a microsecond field and `[-|+]LL` is the positive or negative UTC offset.
* :code:`MYSQL` emulation: :code:`YYYY-MM-DD hh:mm:ss`

Note that NOW is a synonym for CURRENT_TIMESTAMP, but, unlike the latter function, it must always include parentheses.

++++++
NULLIF
++++++

.. code-block:: SQL

   NULLIF(value_expression, value_expression)

The built-in NULLIF function returns NULL if both arguments are equal, or the first argument otherwise.
The arguments must have the same type.

.. The following function is not currently documented because it is not fully functional, but is partially implemented to avoid syntax errors during SQL client startup.
    +++++++++++++++++++
    PG_ENCODING_TO_CHAR
    +++++++++++++++++++

    .. code-block:: SQL

       PG_ENCODING_TO_CHAR(INTEGER)

    The built-in PG_ENCODING_TO_CHAR function converts the value of the current character encoding setting from INTEGER representation to VARCHAR.

    Since PostgreSQL encodings are not fully supported by Octo, this function will always return SQL_ASCII.

.. The following function is not currently documented because it is not fully functional, but is partially implemented to avoid syntax errors during SQL client startup.
    +++++++++++++++++
    PG_IS_IN_RECOVERY
    +++++++++++++++++

    .. code-block:: SQL

       PG_IS_IN_RECOVERY()

    The built-in PG_IS_IN_RECOVERY function returns true if the database is in the process of recovering from a failure by restoring a backup. Since Octo doesn't currently support this feature, this function always returns false (0).

.. The following function is not currently documented because it is not fully functional, but is partially implemented to avoid syntax errors during SQL client startup.
    ++++++++++++++++++++++++
    PG_IS_XLOG_REPLAY_PAUSED
    ++++++++++++++++++++++++

    .. code-block:: SQL

       PG_IS_XLOG_REPLAY_PAUSED()

    The built-in PG_IS_XLOG_REPLAY_PAUSED function returns true if the database has paused the process of recovering from a failure by restoring a backup. Since Octo doesn't currently support this feature, this function always returns false (0).

+++++++
ROUND
+++++++

.. code-block:: SQL

   ROUND(NUMERIC, INTEGER)

ROUND returns the first argument rounded to the precision specified by the second argument.
If the precision is greater than zero, the number will be rounded to that number of decimal places.
If the precision is zero, it will be rounded to the nearest integer.
If the precision is less than zero, all fractional digits will be truncated and the number will be rounded to `10^precision`.
The precision must be no less than -46.

++++++++++++
SESSION_USER
++++++++++++

.. code-block:: SQL

   SESSION_USER
   SESSION_USER()

The built-in SESSION_USER function returns the name of the current session user. However, since Octo currently does not support session users, this function is an alias for CURRENT_USER.

++++++++++++++++
TRUNC/TRUNCATE
++++++++++++++++

.. code-block:: SQL

   TRUNC(NUMERIC, INTEGER)
   TRUNC(NUMERIC, NUMERIC)
   TRUNC(INTEGER, NUMERIC)
   TRUNC(INTEGER, INTEGER)
   TRUNCATE(NUMERIC, INTEGER)
   TRUNCATE(NUMERIC, NUMERIC)
   TRUNCATE(INTEGER, NUMERIC)
   TRUNCATE(INTEGER, INTEGER)

TRUNC (or TRUNCATE) returns the first argument truncated to the precision specified by the second argument.
If the precision is greater than zero, the number will be truncated to that number of decimal places.
If the precision is zero, this behaves the same as the mathematical `floor` function.
If the precision is less than zero, all fractional digits will be truncated and the number will be truncated to `10^precision`.
The precision must be no less than -43.

--------------
Constructors
--------------

-----
ARRAY
-----

.. code-block:: SQL

   ARRAY(single_column_subquery)

The ARRAY constructor can be used to generate a single-dimensional array from the results of a subquery, with each result row value occupying one element of the array. The subquery must return only one column.

.. note::
   The array data type is not currently supported and the constructed array is in fact treated as a string in Octo. As a result, multi-dimensional arrays cannot be constructed using this syntax. Similarly, syntax and functions that rely on the array data type are also unsupported.

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

.. _sql-alias:

------------------------
Alias
------------------------

Double quotes, single quotes and non quoted identifiers can be used to represent alias names.

++++++++++++++
Column Alias
++++++++++++++

A column alias can be used in two different ways:

  #. **As part of SELECT**

     .. code-block:: SQL

        SELECT column [AS] column_alias
	FROM from_item;

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

  #. **As part of FROM**

     .. code-block:: SQL

        SELECT [ALL | DISTINCT]
	[* | expression]
	FROM table_name [AS] table_alias(column_alias [, ...]);

     Examples:

     .. code-block:: SQL

	OCTO> SELECT * FROM names AS tblalias(colalias1, colalias2, colalias3) WHERE tblalias.colalias1 = 1;
        1|Acid|Burn

.. _sql-table-alias:

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

   /* The select subquery uses aliases for the table as well as columns. This query selects one row from the names table aliased as tblalias, where the value of the colalias1 is one(1). */
   OCTO> SELECT * FROM (SELECT * FROM names) as tblalias(colalias1, colalias2, colalias3) WHERE tblalias.colalias1 = 1;
   1|Acid|Burn

Table aliases are supported in short form i.e without AS

.. code-block:: SQL

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

The value expression can contain an unsigned value, a column reference, a set function, a subquery or :code:`table_name.*`

:code:`table_name.*` usage:

* When :code:`table_name.*` is used, all columns of the table specified are included
* It can be used in SELECT, GROUP BY, and ORDER BY column list
* It can also be used with set functions in SELECT, HAVING and ORDER BY expressions
* Apart from COUNT other set functions can have :code:`table_name.*` only when the table has a single column and if its type is compatible with the function.
* When :code:`COUNT( [set_quantifier] table_name.* )` is used as a column in SELECT, other columns have to either be present in GROUP BY or should be part of a :code:`set_function` otherwise error is raised for the column not following this condition
* When :code:`table_name.*` is used with COUNT, all columns of the table are considered for processing. In case a row exists where all columns have artificial NULL values, :code:`COUNT(tablename.*)` or :code:`COUNT(DISTINCT tablename.*)` will not include the row in its result. We can end up with such a row when an outer join is used and there is no match for the right table, in this case the rows of the right table in the join will have only artificial NULL values.

general_set_function refers to functions on sets like AVG, SUM, MIN, MAX etc. A set function can also contain the keyword COUNT, to count the number of resulting columns or rows that result from the query.

A query expression can be a joined table or a non joined query expression.

.. code-block:: none

   query_expression: non_join_query_expression | joined_table;

The non_join_query_expression includes simple tables and column lists.

.. _northwind-ddl-ex:

---------------------
Northwind DDL Example
---------------------

The following is a CREATE TABLE statement from the `Northwind database adapted for Octo <https://gitlab.com/YottaDB/DBMS/YDBOcto/-/blob/master/tests/fixtures/northwind.sql>`_.

.. code-block:: SQL

   CREATE TABLE Customers(
     CustomerID INTEGER PRIMARY KEY,
     CustomerName VARCHAR(48),
     ContactName VARCHAR(32),
     Address VARCHAR(64),
     City VARCHAR(32),
     PostalCode VARCHAR(16) NOT NULL,
     Country VARCHAR(32)
   )
   GLOBAL "^Customers(keys(""CustomerID""))";

In the above, the :code:`Customers` table maps data in nodes of the global variable :code:`^Customers`. The columns of the primary key of the table are all subscripts of a global variable node (all columns in the primary key are global variable subscripts; all global variable subscripts are not necessarily columns, as shown by the next example). The :code:`^Customers` global variable has one subscript, an integer mapping to the column :code:`CustomerID`.

Columns such as :code:`CustomerName` are pieces of the node, using the default :code:`"|"` as the piece separator, in the order listed. If PIECE is not specified, Octo maps columns in the order in which they appear in the CREATE TABLE statement to consecutive pieces of the global node value.

As Octo 1.0 is a read-only SQL engine, it ignores the VARCHAR() size limits and reports the actual data in the global variable nodes. They will be used when Octo supports read-write access to databases.

SQL allows columns other than key columns to have a :code:`NULL` value. The :code:`NOT NULL` for the :code:`PostalCode` column tells Octo that this column can never have a :code:`NULL` value. Since Octo uses empty strings to store :code:`NULL` in the global variable nodes, this means that there can never be a global variable node in the :code:`^Customers` global with an empty string as the fifth piece.

---------------------
VistA DDL Example 1
---------------------

The following is a CREATE TABLE for the :code:`INDEX_DESCRIPTION` table of a `VistA <https://en.wikipedia.org/wiki/VistA>`_ environment. This illustrates how part of a global variable tree is mapped to a table, i.e., different parts of a different global variable tree can potentially be mapped to different tables.

.. code-block:: SQL

   CREATE TABLE `INDEX_DESCRIPTION`(
    `INDEX_ID` NUMERIC PRIMARY KEY START 0 END "'(keys(""INDEX_ID""))!(keys(""INDEX_ID"")="""")",
    `INDEX_DESCRIPTION_ID` NUMERIC KEY NUM 1 START 0 END "'(keys(""INDEX_DESCRIPTION_ID""))!(keys(""INDEX_DESCRIPTION_ID"")="""")",
    `DESCRIPTION` VARCHAR GLOBAL "^DD(""IX"",keys(""INDEX_ID""),.1,keys(""INDEX_DESCRIPTION_ID""),0)"
       EXTRACT "$G(^DD(""IX"",keys(""INDEX_ID""),.1,keys(""INDEX_DESCRIPTION_ID""),0))"
   )
   GLOBAL "^DD(""IX"",keys(""INDEX_ID""),.1,keys(""INDEX_DESCRIPTION_ID""))";

The table has a numeric primary key, :code:`INDEX_ID`. :code:`START 0` means that a :code:`$ORDER()` loop to find the next subscript starts with :code:`0` and :code:`END "'(keys(""INDEX_DESCRIPTION_ID""))!(keys(""INDEX_DESCRIPTION_ID"")="""")"` means that the loop ends when the result of that :code:`$ORDER()` is :code:`0` or the empty string (:code:`""`), indicating the end of breadth first traversal of that level of the tree.

:code:`GLOBAL "^DD(""IX"",keys(""INDEX_ID""),.1,keys(""INDEX_DESCRIPTION_ID""))"` means that the table is in multiple :code:`^DD("IX",…,.1,…)` subtrees of :code:`^DD` with the primary key :code:`INDEX_ID` in the second subscript, and the :code:`INDEX_DESCRIPTION_ID` column in the fourth subscript, with :code:`.1` as the third subscript. GLOBAL can also be applied at the COLUMN level to allow a table to incorporate columns from different global variables, with the restriction that KEY columns of a table must all be subscripts of the same global variable.

The :code:`DESCRIPTION` column is a text field, whose value is the entire global variable node. Unlike the previous example, the global variable node is not piece separated columns. EXTRACT in a column specification overrides any implicit or explicit PIECE specification for that column.

The backtick character (:code:`"\`"`) is used to enclose words so that any possible reserved words that may be used in column or table names are correctly escaped. [Note, the backslash works around a limitation of the publishing software; it is not part of the backtick character.]

---------------------
VistA DDL Example 2
---------------------

The following is another example from a VistA environment, automatically generated by the `VistA Fileman to Octo DDL mapping tool <https://gitlab.com/YottaDB/DBMS/YDBOctoVistA>`_.

.. code-block:: SQL

   CREATE TABLE `LINE_PORT_ADDRESS`(
    `LINE_PORT_ADDRESS_ID` NUMERIC PRIMARY KEY START 0 END "'(keys(""LINE_PORT_ADDRESS_ID""))!(keys(""LINE_PORT_ADDRESS_ID"")="""")",
    `NAME` CHARACTER(30) NOT NULL GLOBAL "^%ZIS(3.23,keys(""LINE_PORT_ADDRESS_ID""),0)" PIECE 1,
    `LOCATION` CHARACTER(30) GLOBAL "^%ZIS(3.23,keys(""LINE_PORT_ADDRESS_ID""),0)" PIECE 2,
    `DEVICE` INTEGER GLOBAL "^%ZIS(3.23,keys(""LINE_PORT_ADDRESS_ID""),0)" PIECE 3,
    `SUBTYPE` INTEGER GLOBAL "^%ZIS(3.23,keys(""LINE_PORT_ADDRESS_ID""),0)" PIECE 4
   )
   GLOBAL "^%ZIS(3.23,keys(""LINE_PORT_ADDRESS_ID""))"
   DELIM "^";

:code:`DELIM "^"` specifies to Octo that :code:`"^"` is the piece separator to use when mapping values of global variable nodes into columns.

As with the :code:`PostalCode` column from the :ref:`northwind-ddl-ex` above, the NOT NULL for the :code:`NAME` column means that an empty string for the first piece of :code:`^%ZIS(3.23,…)` global variable nodes will be treated as an empty string rather than a NULL. In contrast, had the INTEGER :code:`DEVICE` column been declared NOT NULL, an empty string for the third piece of global variable nodes would have been reported as a zero rather than a NULL.

.. _sqlnull:

---------------------
SQL NULL Values
---------------------

Octo treats every empty string (:code:`''`) specified in a query as if :code:`NULL` was instead specified. This differs from Postgres where empty strings and :code:`NULL` are treated differently. Therefore queries that use empty strings will most likely need to be examined and reworded to instead use :code:`NULL`.

For example, :code:`select * from names where lastname = ''` is equivalent to :code:`select * from names where lastname = NULL`. And since the check :code:`lastname = NULL` will never evaluate to :code:`TRUE`, the query should instead be reworded as :code:`select * from names where lastname is NULL` to return the intended results.

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
   GLOBAL "^USAddresses";

The second piece of the node, which corresponds to the :code:`AddressLine2` column, is an empty string (:code:`''` in SQL). In this case, Octo treats the :code:`AddressLine2` column as having a :code:`NULL` value.

