.. #################################################################
.. #								   #
.. # Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.  #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

====================
Configuration
====================

.. contents::
   :depth: 5

Configuration settings can be passed to the program with the following precedence:

  1. Flags passed to the program
  2. octo.conf
  3. ~/octo.conf
  4. $ydb_dist/plugin/octo/octo.conf
  5. Environment settings

---------------------
Environment variables
---------------------

  The following environment variables must be set:

    * ydb_dist
    * ydb_gbldir
    * ydb_routines

  The environment variables :code:`ydb_dist`, :code:`ydb_gbldir`, and :code:`ydb_routines` can initially be set by sourcing :code:`ydb_env_set` in your YottaDB installation directory.

  Example setting of the environment variables (assuming default paths):

   .. code-block:: bash

      source /usr/local/lib/yottadb/r1.28/ydb_env_set
      export ydb_routines=". $ydb_routines"

--------------------------------
Global Variables
--------------------------------

  All Octo related globals are prefixed with :code:`^%ydbocto`. Using normal global mapping procedures for an existing application global directory (where you want to run Octo), map the global variable namespace :code:`^%ydbocto*` to a separate region (and its associated database file) that meets the below requirements (the below example commands assume the separate region is named :code:`OCTO`).

    * :code:`NULL_SUBSCRIPTS` must be set to :code:`ALWAYS`.

    * :code:`KEY_SIZE` must be tuned to your data - this can be set to the maximum allowed by YottaDB - "1019"

    * :code:`RECORD_SIZE` must be tuned to your data/queries - a reasonable starting value is "300000"

  Example:

  .. code-block:: bash

     GDE> add -segment OCTO -access_method=BG -file_name="$ydb_dir/$ydb_rel/g/octo.dat"
     GDE> add -region OCTO -dynamic=OCTO -null_subscripts=ALWAYS -key_size=1019 -record_size=300000
     GDE> add -name %ydbocto* -region=OCTO

  Some of the globals used in Octo are:

    * :code:`^%ydboctoocto`: This global can refer to various functions, variables, Octo "read only" table values (postgres mappings, oneRowTable, etc.), and some counts. It needs to persist between sessions.

    * :code:`^%ydboctoxref`: This global contains cross-references, row counts and other statistical information. It needs to persist between sessions.

    * :code:`^%ydboctoschema`: This global contains information about the tables loaded into the database. It needs to persist between sessions.

  Since most of the Octo variables need to persist between sessions, it is necessary that the region(s) mapping the :code:`^%ydbocto*` namespace have replication turned on in a replicated environment (or journaling turned on in a non-replicated environment).

--------------------
Config files
--------------------

  Octo currently looks for a configuration file in the following directories:

    * $ydb_dist/plugin/octo/octo.conf
    * ~/octo.conf
    * ./octo.conf

  If the same setting exists in more than one configuration file the setting in the later file (according to the list above) will prevail. An example config file can be found in :code:`$ydb_dist/plugin/octo/octo.conf`.

  Sample config file:

  .. literalinclude:: ../src/aux/octo.conf.default

  A few of the configuration settings are described below.

~~~~~~~~~~~~~~~
emulate
~~~~~~~~~~~~~~~

  Octo supports partial emulation of multiple SQL database products. The :code:`emulate` configuration option may be used to specify which SQL database Octo will attempt emulate at process initialization time. Currently supported options are: :code:`POSTGRES` and :code:`MYSQL`. The default is :code:`POSTGRES`. If you wish to emulate MariaDB, choose MYSQL.

~~~~~~~~~~~~~~~
octo_zroutines
~~~~~~~~~~~~~~~

  Octo requires that :code:`$ydb_dist/plugin/o/_ydbocto.so` and :code:`$ydb_dist/plugin/o/_ydbposix.so` (:code:`$ydb_dist/plugin/o/utf8/_ydbocto.so` and :code:`$ydb_dist/plugin/o/utf8/_ydbposix.so` when using Octo in YottaDB's UTF-8 mode) be included in :code:`$ydb_routines`. This is necessary not only for running the :code:`octo` and :code:`rocto` executables, but also for correctly updating and maintaining the YottaDB triggers that are used to maintain cross references for Octo. Accordingly these paths should exist in :code:`ydb_routines` in your normal environment setup scripts.

  The :code:`octo_zroutines` configuration setting allows one to prefix :code:`ydb_routines` env var with one or more paths specified in the configuration file.

  .. note::

     The :code:`source /usr/local/etc/ydb_env_set` command sets these up automatically for environments with the default structure under :code:`$ydb_dir` (defaulting to :code:`$HOME/.yottadb`).

~~~~~~~~~~~~~
tabletype
~~~~~~~~~~~~~

  The :code:`tabletype` configuration setting determines the type of a table. It can take on one of the following values.

    * :code:`"READONLY"`: This table type allows greater flexibility in mapping SQL tables to pre-existing M global nodes by allowing the specification of keywords like :code:`START`, :code:`STARTINCLUDE`, :code:`END`, :code:`PIECE`, :code:`GLOBAL` or column-level :code:`DELIM` in the :code:`CREATE TABLE` command. But queries that update tables like :code:`INSERT INTO`, :code:`DELETE FROM` etc. are not allowed in this type of table.
    * :code:`"READWRITE"`: This table type does not offer as much flexibility as :code:`READONLY` type of tables in mapping SQL tables to pre-existing M global nodes. Keywords like :code:`START`, :code:`END` are not allowed in a :code:`CREATE TABLE` command if :code:`READWRITE` table type is also specified. But queries that update tables like :code:`INSERT INTO`, :code:`DELETE FROM` etc. are allowed in this type of table.

  If the :code:`tabletype` configuration setting is unspecified in the configuration file, :code:`READWRITE` is the default value.

  A :code:`DROP TABLE` command on a :code:`READWRITE` table drops the table as well as kills all underlying global nodes that stored the table data. On the other hand, a :code:`DROP TABLE` command on a :code:`READONLY` table only drops the table and leaves the underlying global nodes that stored the table data untouched.

~~~~~~~~~~~~~
verbosity
~~~~~~~~~~~~~

  The :code:`verbosity` configuration setting controls logging verbosity. It can take on one of the following values.

    * :code:`"TRACE"`: A TRACE message is useful to help identify issues in the code. The equivalent verbosity option is :code:`--verbose=3`.
    * :code:`"INFO"`: An INFO message is used to relay information to the user. The equivalent verbosity option is :code:`--verbose=2`.
    * :code:`"DEBUG"`: A DEBUG message helps users debug configuration issues. The equivalent verbosity option is :code:`--verbose=1`.
    * :code:`"ERROR"` : An ERROR message informs the user that an error has occurred. The equivalent verbosity option is :code:`--verbose=0`.

  See :ref:`Verbose Launching Option <verbose-option>` for more information on verbosity levels.

~~~~~~~~~~~~
octo_history
~~~~~~~~~~~~

  Octo History location. If not specified, it will be in :code:`~/.octo_history` by default. You can override it here.

~~~~~~~~~~~~~~~~~~~~~~~
octo_history_max_length
~~~~~~~~~~~~~~~~~~~~~~~

  Maximum number of entries to save for Octo history. If not specified, it will be 500 by default. Note: while Octo is running, there is no history trimming being performed, so you can have more history while running Octo than the limit specified here.

-----------------------
TLS/SSL Configuration
-----------------------

  .. note::
     The instructions provided in this section will help in setting up a :code:`self-signed` certificate for TLS, but *not* for setting up TLS in production.
     Also, a full TLS setup will require certificates signed by a known and trusted certificate authority.

  Enabling TLS/SSL requires several additional steps beyond installing the YottaDB encryption plugin - it requires creating a Certificate Authority (CA), generating a TLS/SSL certificate, and making additional changes to :code:`octo.conf`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generate CA key and certificate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  .. code-block:: bash

     # In a directory in which you want to store all the certificates for Octo
     # Be sure to create a strong passphrase for the CA
     openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out CA.key
     # This creates a CA valid for 1-year and interactively prompts for additional information
     openssl req -new -nodes -key CA.key -days 365 -x509 -out CA.crt

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Create server key and certificate request
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  .. code-block:: bash

     # This creates a 2048 bit private key
     openssl genpkey -algorithm RSA -pkeyopt rsa_keygen_bits:2048 -out server.key
     # This creates the certificate signing request
     openssl req -new -key server.key -out server.csr

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sign certificate based on request and local CA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  .. code-block:: bash

     # Asks the CA to sign the certificate with a 1-Year validity time
     openssl x509 -req -in server.csr -CA CA.crt -CAkey CA.key -CAcreateserial -out server.crt -days 365
     # Mask the password for the certificate in a way YottaDB understands
     $ydb_dist/plugin/gtmcrypt/maskpass
     # This will need to be added to any startup scripts for octo/rocto
     export ydb_tls_passwd_OCTOSERVER=[Masked Password from maskpass]
     export ydb_crypt_config=/path/to/octo.conf

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Update Octo configuration file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  $ydb_dist/plugin/octo/octo.conf contains an outline of the minimum configuration settings.

  For TLS/SSL a configuration file is required, based on the changes below.

    1. In the :code:`rocto` section, the :code:`ssl_on` configuration setting must be set to :code:`true`.
    2. A :code:`tls` section must be present and generally conform to the requirements specified for the `TLS plugin itself <https://docs.yottadb.com/AdminOpsGuide/tls.html>`_. Other notes:

        * Octo doesn't use any of the :code:`dh*` settings, so those can be omitted.
        * The :code:`format` specifier can also be omitted, as long as the certs are in PEM format.
        * The :code:`CAfile` and :code:`CApath` fields are mandatory and must point to valid files/locations with a full path.
        * A subsection named :code:`OCTOSERVER` with :code:`key`, and :code:`cert` settings specifying the names of the private key and cert files.

    3. The :code:`ydb_tls_passwd_OCTOSERVER` and :code:`ydb_crypt_config` environment variables must be set correctly.

  If you source :code:`/usr/local/etc/ydb_env_set` it provides reasonable default values of environment variables. Review the :code:`$ydb_dist/plugin/octo/octo.conf` file to configure your own environment.

----------------------------
Usage
----------------------------

  Before running Octo/ROcto make sure that the required YottaDB variables are set either by creating your own script or run :code:`source /usr/local/etc/ydb_env_set`.

  To use the command-line SQL interpreter run: :code:`$ydb_dist/plugin/bin/octo`.

  To use rocto, the PostgreSQL protocol compatible server, run :code:`$ydb_dist/plugin/bin/rocto -p XXXX` where :code:`-p XXXX` optionally specifies a TCP port at which rocto is to listen for connections. The default port number is 1337.
