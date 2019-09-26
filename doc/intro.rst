
====================
Introduction
====================

.. contents::
   :depth: 3

This manual documents the YottaDB Database Management System i.e Octo.

The YottaDB Database Management System (Octo) is a layered application with a relational access model, built on top of the not-only-SQL database YottaDB. It aims to provide SQL 92 compliance and exceptional performance.

--------------------
Setup
--------------------

YottaDB r1.26 or greater is required for successful installation of Octo.

Installing and configuring YottaDB is described in the `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/installydb.html>`_.

.. note::
   It is required that the environment variable :code:`$ydb_dist` is defined - :code:`$gtm_dist` is not a valid subsitute.

++++++++++++++++++++++++++++++++++
Quickstart - Install from Source
++++++++++++++++++++++++++++++++++

* Install YottaDB POSIX plugin

  More detailed instructions are on the YottaDB `POSIX plugin README <https://gitlab.com/YottaDB/Util/YDBposix/blob/master/README.md>`_.

  .. parsed-literal::

     curl -fSsLO https://gitlab.com/YottaDB/Util/YDBposix/-/archive/master/YDBposix-master.tar.gz
     tar xzf YDBposix-master.tar.gz
     cd YDBposix-master
     mkdir build && cd build
     \# Make sure that you have YottaDB environment variables in your shell before continuing
     cmake ..
     make && sudo make install

* (Optional) Install YottaDB encryption plugin

  Installing the YottaDB encryption plugin enables TLS support (Recommended for production installations). You will need to make sure TLS/SSL is enabled for the driver in the client software chosen.

  .. parsed-literal::

     \# In a temporary directory perform the following commands
     sudo tar -xf $ydb_dist/plugin/gtmcrypt/source.tar
     \# Make sure that you have YottaDB environment variables in your shell before continuing
     sudo ydb_dist=$ydb_dist make
     sudo ydb_dist=$ydb_dist make install

* Install prerequisite packages

  .. parsed-literal::

     \# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
     sudo apt-get install build-essential cmake bison flex xxd libreadline-dev libssl-dev

     \# CentOS Linux OR RedHat Linux
     \# Note: epel-release has to be installed before cmake3 is installed
     sudo yum install epel-release
     sudo yum install cmake3 bison flex readline-devel vim-common libconfig-devel openssl-devel

* Download Octo Source Code

  .. parsed-literal::

     curl -fSsLO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/archive/master/YDBOcto-master.tar.gz
     tar xzf YDBOcto-master.tar.gz
     cd YDBOcto-master

* Compile Octo

  .. parsed-literal::

     mkdir build
     cd build
     \# For VistA the String Buffer Length needs to be larger (described below) add "-DSTRING_BUFFER_LENGTH=300000" to the cmake command below
     cmake -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin .. # for CentOS/RedHat use cmake3 instead
     make

* Install Octo

  .. parsed-literal::

     make install

++++++++++++++++++++++++++
Optional CMAKE Parameters
++++++++++++++++++++++++++

Octo uses some cmake parameters to control generation of fixed-size buffer allocations. These are:

* STRING_BUFFER_LENGTH - the maximum length of a string within the system; this supercedes any VARCHAR definitions.
* INIT_M_ROUTINE_LENGTH - the initial length for the buffer of generated M routines. The default is 10MB.
* MEMORY_CHUNK_SIZE - size of memory chunks to allocate; default is 32MB.
* MEMORY_CHUNK_PROTECT - if non-zero, memory following chunks is protected to detect buffer overflows. If 2, data is placed closer to the protected region to increase the chances of detecting an error.

Example usage of the above parameters:

.. parsed-literal::

   cmake -DSTRING_BUFFER_LENGTH=600000 -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin ..

+++++++++++++++++
Usage
+++++++++++++++++

Before running Octo/Rocto make sure that the required YottaDB variables are set either by creating your own script or run :code:`source $ydb_dist/ydb_env_set`.

To use the command-line SQL interpreter run: :code:`$ydb_dist/plugin/bin/octo`.

To use the PostgreSQL protocol compatible server run :code:`$ydb_dist/plugin/bin/rocto`.

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

