
====================
Introduction
====================

.. contents::
   :depth: 3

This manual documents the YottaDB Database Management System i.e Octo.

The YottaDB Database Management System (Octo) is a layered application with a relational access model, built on top of the not-only-SQL database YottaDB. It aims to provide SQL 92 compliance and exceptional performance.

--------------------
Launching Octo
--------------------

Firstly, make sure YottaDB is correctly `downloaded and installed <https://yottadb.com/product/get-started/>`_.

The SQL engine looks for the environment variable ydb_dist.

ydb_dist specifies the path to the directory containing the YottaDB system distribution. Either use the ydb script or source ydb_env_set to define ydb_dist. Correct operation of YottaDB executable programs requires ydb_dist to be set correctly.

i.e.

.. parsed-literal::
   source /usr/local/lib/yottadb/r124/ydb_env_set

Obtain Octo from our `repository on GitLab <https://gitlab.com/YottaDB/DBMS/YDBOcto>`_.

.. parsed-literal::
   wget https://gitlab.com/YottaDB/DBMS/YDBOcto/-/archive/master/YDBOcto-master.tar.gz

Uncompress the file and move to the directory.

.. parsed-literal::
   tar -xzf YDBOcto*.tar.gz
   cd YDBOcto-master/

Compile Octo using the following steps:

.. parsed-literal::
   mkdir build
   cd build
   cmake ..
   make

To test your build, you can run:

.. parsed-literal::
   make test

Launch Octo (without options) with the following command:

.. parsed-literal::
   octo@test:~$ ./src/octo
   OCTO>

The terminal will return with the OCTO> command prompt.

+++++++++++++++++++
Launching Options
+++++++++++++++++++

Octo has a few options that can be specified when it is launched.

~~~~~~~~~
Verbose
~~~~~~~~~

The verbose option specifies the amount of additional information that is provided to the user when commands are run in Octo.

.. parsed-literal::
   --verbose={number}

or equivalently,

.. parsed-literal::
   -v{v{v}}

The number given to the option corresponds to the following levels:

+-----------------+------------------------+---------------------------------------------+
| Number          | Level                  | Information                                 |
+=================+========================+=============================================+
| 0               | FATAL                  | Informaton about fatal errors               |
+-----------------+------------------------+---------------------------------------------+
| 1               | ERROR                  | Information about all errors                |
+-----------------+------------------------+---------------------------------------------+
| 2               | WARNING                | Includes warnings                           |
+-----------------+------------------------+---------------------------------------------+
| 3               | DEBUG                  | Includes information useful for debugging   |
+-----------------+------------------------+---------------------------------------------+
| 4               | INFO                   | Additional information useful to log        |
+-----------------+------------------------+---------------------------------------------+
| 5               | TRACE                  | Information logged steppping through actions|
+-----------------+------------------------+---------------------------------------------+

When a number level is specified, the verbose output contains all information corresponding to that level as well as the previous levels.

The default verbose level is set to 2 (WARNING).

A single -v in the command line puts the verbose level at 3, -vv puts the level at 4, and -vvv puts the level at 5.

Example:

.. parsed-literal::
   octo --verbose=4

Example:

.. parsed-literal::
   OCTO> YDBOcto-master/build $ ./src/octo -vvv
   [TRACE] YDBOcto-master/src/octo.c:50 2019-04-10 10:17:57 : Octo started
   [ INFO] YDBOcto-master/src/run_query.c:79 2019-04-10 10:17:57 : Generating SQL for cursor 45
   [ INFO] YDBOcto-master/src/run_query.c:81 2019-04-10 10:17:57 : Parsing SQL command
   Starting parse
   Entering state 0
   Reading a token: OCTO> Next token is token ENDOFFILE (: )
   Shifting token ENDOFFILE (: )
   Entering state 15
   Reducing stack by rule 8 (line 182):
      $1 = token ENDOFFILE (: )
   Stack now 0
   [ INFO] YDBOcto-master/src/run_query.c:83 2019-04-10 10:18:00 : Done!
   [ INFO] YDBOcto-master/src/run_query.c:89 2019-04-10 10:18:00 : Returning failure from run_query

~~~~~~~~
Dry-run
~~~~~~~~

The dry-run option runs the parser, and performs checks and verifications on data types and syntax, but does not execute the SQL statements. The database is not altered when Octo is run with the --dry-run option.

.. parsed-literal::
   --dry-run

or equivalently,

.. parsed-literal::
   -d

Example:

.. parsed-literal::
   octo --dry-run

~~~~~~~~~~~
Input-file
~~~~~~~~~~~

The input-file option takes a file as input to Octo, that commands are then read from.

.. parsed-literal::
   --input-file=<path to input file>

or equivalently,

.. parsed-literal::
   -f <input file>

Example:

.. parsed-literal::
   octo --input-file=files/commands.txt

~~~~~~~~~~
-k
~~~~~~~~~~

This command line argument overrides the :code:`auto_clean_tables` configuration setting and instructs Octo to not cleanup temporary tables (stored in the :code:`ydbcursor` global) after data is emitted to the client. This is useful for developers to debug problems.

Example:

.. parsed-literal::
   octo -k
