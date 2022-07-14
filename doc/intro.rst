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

====================
Introduction
====================

.. contents::
   :depth: 5

This manual documents Octo, the YottaDB Database Management System.

Octo is a SQL database engine whose tables are stored in YottaDB global variables (i.e., YottaDB hierarchical key-value nodes). Octo is installed as a YottaDB plugin.

* It is quick and efficient in pulling data from the YottaDB datastore.
* It is tightly integrated with the YottaDB object technology that allows for a mix of relational as well as object access to the YottaDB datastore seamlessly. It does not sacrifice one for the other.
* It aims to provide SQL-92 compliance.

-------------
Architecture
-------------

  * Octo uses the PostgreSQL wire protocol, allowing SQL access to YottaDB databases via the PostgreSQL ODBC/JDBC/OLE DB driver.
  * It uses YottaDB local and global variables to hold mapping data, temporary tables, and cross references to provide an efficient relational schema overlay using an augumented SQL DDL language.
  * It uses a 3-phase architecture, consisting of parsing, logical-plan generation and optimization, and physical-plan generation and emission.
  * Rocto is the Remote Octo Server that can communicate with PostgreSQL Server clients.

-------------------
Features
-------------------

  Octo includes a full set of standard, relational features. These include:

  * The ability to define data structures, especially database schemas (Data Definition Language, or DDL).
  * The ability to retrieve data (Data Query Language, or DQL).
  * The ability to insert/update data (Data Manipulation Language, or DML).
    
  .. note::

     At the time of the release of this document, the features that manage transactions in the database (Transaction Control Language, or TCL), and control access to data stored in a database (Data Control Language, or DCL) are yet to be implemented.

--------------------
Setup
--------------------

  YottaDB r1.34 or greater is required for successful installation of Octo.

  Installing and configuring YottaDB is described on its own `documentation page <https://docs.yottadb.com/AdminOpsGuide/installydb.html>`__. With the :code:`--octo` option of YottaDB's `ydbinstall.sh <https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh>`_ script, you can install YottaDB and Octo with one command.

  .. note::
    Octo is a YottaDB application, not an application that runs on the upstream GT.M for which YottaDB is a drop-in upward-compatible replacement.

-------------
Quickstart
-------------

++++++++++++++++++++++
Install Prerequisites
++++++++++++++++++++++

~~~~~~~~~~~~~~~~~~~
  Install Plugins
~~~~~~~~~~~~~~~~~~~

  The YottaDB POSIX and AIM plugins are now installed when the :code:`--octo` option is used with the :code:`ydbinstall` script.

  Installing the YottaDB encryption plugin enables TLS support (Recommended for production installations). You will need to make sure TLS/SSL is enabled for the driver in the client software chosen.

  The YottaDB encryption plugin can be installed by adding the :code:`--encplugin` option when installing YottaDB with the :code:`ydbinstall` script:

  .. code-block:: bash

     ./ydbinstall --encplugin

.. note::

   If YottaDB has already been installed, use the :code:`--plugins-only` option with the ydbinstall.sh script to install the plugins.

++++++++++++
Install Octo
++++++++++++

  Octo is a continuously updated YottaDB plugin that is distributed as source code. A CI (Continuous Integration) pipeline runs a considerable number of unit and system tests before allowing any source code to be merged. This ensures that the master branch is always current with the latest production-ready source code. Octo can be installed by using the :code:`--octo` option when installing YottaDB with the :code:`ydbinstall` script. Alternatively, you can build it from source.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     Install Prerequisite Packages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

     .. code-block:: bash

        # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
        sudo apt-get install --no-install-recommends build-essential cmake bison flex xxd libreadline-dev libssl-dev wget ca-certificates file libelf-dev curl git pkg-config libicu-dev libconfig-dev

        # RHEL 8/Rocky Linux
        yum --enablerepo=powertools install -y gcc make cmake bison flex readline-devel git libconfig-devel pkg-config libicu-devel wget vim findutils procps file openssl-devel postgresql

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   *(Optional)* Prerequisites for Automated Regression Testing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Refer to the `Developer's Documentation <../developer_doc.html>`_ for information on setting up and running automated regression testing.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     Clone the Octo source code repository
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      .. code-block:: bash

         # In a temporary directory perform the following commands
         git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git
         cd YDBOcto

~~~~~~~~~~~~~~~~~
     Compile Octo
~~~~~~~~~~~~~~~~~

      .. code-block:: bash

         mkdir build ; cd build
         cmake ..
         make -j `getconf _NPROCESSORS_ONLN`

      Most users proceed to the *Install Octo* step below. The instructions here are for those wishing to contribute to Octo, or building it on Supportable but not Supported platforms.

~~~~~~~~~~~~~~~~~
     Install Octo
~~~~~~~~~~~~~~~~~

      Install Octo:

      .. code-block:: bash

         # Tell git that this repo is safe to run git in as root
         # Don't forget the -E with sudo; otherwise the git config command has no effect
         git config --global --add safe.directory $(git rev-parse --show-toplevel)
         sudo -E make install

      .. note::

	 New Octo installations include a default :code:`octo.conf` configuration file at :code:`$ydb_dist/plugin/octo/octo.conf`, which may be modified post-install. Re-installing Octo will *not* overwrite an existing :code:`octo.conf` in this location, so modifications to this file will be preserved across installations.

++++++++++++++++
Configure Octo
++++++++++++++++

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Setup environment variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The following environment variables must be set for Octo to operate properly:

      * :code:`ydb_dist`
      * :code:`ydb_gbldir`
      * :code:`ydb_routines`
      * :code:`ydb_xc_ydbposix`
      * :code:`ydb_xc_octo`
      

  The environment variables :code:`ydb_dist`, :code:`ydb_gbldir`, :code:`ydb_routines`, and :code:`ydb_xc_ydbposix` can initially be set by sourcing :code:`ydb_env_set` in your YottaDB installation directory.

  Example setting of the environment variables (assuming default paths):

  .. code-block:: bash

     source /usr/local/lib/yottadb/r1.34/ydb_env_set
     export ydb_xc_octo=$ydb_dist/plugin/octo/ydbocto.xc  # Allow usage of some SQL functions, e.g. DATE_FORMAT()

~~~~~~~~~~~~~~~~
  Setup Database
~~~~~~~~~~~~~~~~

.. note::

   There is no need to create databases manually if :code:`ydb_env_set` has been sourced.
   
  Octo uses several global variables for its operation, which start with :code:`%ydbocto` and :code:`%ydbAIM`. The :code:`%ydbAIM` globals are intended to be ephemeral and are not recommended to be journaled; we also recommend that you use a memory mapped region with 2K blocks. Use `GDE <https://docs.yottadb.net/AdminOpsGuide/gde.html>`_ to map :code:`%ydbocto*` and :code:`%ydbAIM` global variables to a separate region. Global variables used by Octo and AIM must have `NULL_SUBSCRIPTS=ALWAYS <https://docs.yottadb.net/AdminOpsGuide/gde.html#no-n-ull-ubscripts-always-never-existing>`_.

  The following example creates an OCTO database region with the recommended setting in the :code:`$ydb_dir/$ydb_rel/g` directory and assumes an existing application global directory at :code:`$ydb_dir/$ydb_rel/g/yottadb.gld`. For more information on setting up a database in YottaDB, refer to the `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/index.html>`_, and the `YottaDB Acculturation Guide <https://docs.yottadb.com/AcculturationGuide/>`_ for self-paced exercises on YottaDB DevOps.

  .. code-block:: bash

     $ echo $ydb_dir $ydb_rel
     /tmp/test r1.30_x86_64
     $ $ydb_dist/yottadb -run GDE
     %GDE-I-LOADGD, Loading Global Directory file
             /tmp/test/r1.30_x86_64/g/yottadb.gld
     %GDE-I-VERIFY, Verification OK


     GDE> add -segment OCTO -access_method=BG -file_name="$ydb_dir/$ydb_rel/g/octo.dat"
     GDE> add -region OCTO -dynamic=OCTO -null_subscripts=ALWAYS -key_size=1019 -record_size=300000 -journal=(before,file="$ydb_dir/$ydb_rel/g/octo.mjl")
     GDE> add -name %ydbocto* -region=OCTO
     GDE> add -segment AIM -access_method=MM -allocation=20000 -block_size=2048 -extension_count=20000 -file_name="$ydb_dir/$ydb_rel/g/aim.dat"
     GDE> add -region AIM -dynamic=AIM -null_subscripts=ALWAYS -key_size=992 -record_size=1008
     GDE> add -name %ydbAIM* -region=AIM
     GDE> verify
     %GDE-I-VERIFY, Verification OK


     GDE> exit
     %GDE-I-VERIFY, Verification OK

     %GDE-I-GDUPDATE, Updating Global Directory file
             /tmp/test/r1.30_x86_64/g/yottadb.gld
     $ $ydb_dist/mupip create -region=OCTO
     %YDB-I-DBFILECREATED, Database file /tmp/test/r1.30_x86_64/g/octo.dat created
     $ $ydb_dist/mupip create -region=AIM
     %YDB-I-DBFILECREATED, Database file /tmp/test/r1.30_x86_64/g/aim.dat created
     $ $ydb_dist/mupip set -journal=before,enable,on -region OCTO
     %YDB-I-JNLCREATE, Journal file /tmp/test/r1.30_x86_64/g/octo.mjl created for region OCTO with BEFORE_IMAGES
     %YDB-I-JNLSTATE, Journaling state for region OCTO is now ON
     $

  The commands in the example above are reproduced below, to facilitate copying and pasting.

  .. code-block:: bash

     echo $ydb_dir $ydb_rel
     $ydb_dist/yottadb -run GDE
     add -segment OCTO -access_method=BG -file_name="$ydb_dir/$ydb_rel/g/octo.dat"
     add -region OCTO -dynamic=OCTO -null_subscripts=ALWAYS -key_size=1019 -record_size=300000 -journal=(before,file="$ydb_dir/$ydb_rel/g/octo.mjl")
     add -name %ydbocto* -region=OCTO
     add -segment AIM -access_method=MM -allocation=20000 -block_size=1024 -extension_count=20000 -file_name="$ydb_dir/$ydb_rel/g/aim.dat"
     add -region AIM -dynamic=AIM -null_subscripts=ALWAYS -key_size=992 -record_size=1008
     add -name %ydbAIM* -region=AIM
     verify
     exit
     $ydb_dist/mupip create -region=OCTO
     $ydb_dist/mupip create -region=AIM
     $ydb_dist/mupip set -journal=before,enable,on -region OCTO

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  *(Optional)* Test with dummy data using Octo
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  You can use the `Northwind <https://docs.yottadb.com/Octo/grammar.html#northwind-ddl-example>`_ sample database to get started. The dummy data set can be found in the :code:`tests/fixtures` subdirectory of the YDBOcto repository created by :code:`git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git`.

  A dummy data set consists of a :code:`.zwr` file and a :code:`.sql` file. The former contains the actual data to be stored in YottaDB, while the latter contains a schema that maps relational SQL structures (tables and columns) to the NoSQL data contained in YottaDB. Assuming that :code:`/tmp/YDBOcto` is the directory from the :code:`git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git` command :

   .. code-block:: bash

      # Source ydb_* variables:
      source $(pkg-config --variable=prefix yottadb)/ydb_env_set
      # ydb_dir can optionally be set to use a location other than $HOME/.yottadb for the working environment.

      mupip load /tmp/YDBOcto/build/tests/fixtures/northwind.zwr
      octo -f /tmp/YDBOcto/build/tests/fixtures/northwind.sql

  Once loaded, you can run `octo` to start the Octo interactive shell and use `SELECT <https://docs.yottadb.com/Octo/grammar.html#select>`_ queries to access the data.

^^^^^^^^^^^^^^
Sample Queries
^^^^^^^^^^^^^^

    Given below are some sample queries that can be run in Octo once the :code:`northwind` data set has been loaded.

    The following query selects only the DISTINCT values from the 'Country' column in the 'Suppliers' table.

    .. code-block:: SQL

     OCTO> SELECT DISTINCT Country FROM Suppliers;
     UK
     USA
     Japan
     Spain
     Australia
     Sweden
     Brazil
     Germany
     Italy
     Norway
     Sweden
     France
     Singapore
     Denmark
     Netherlands
     Finland
     Canada

    The following query selects the first five records from the 'Customers' table where the country is 'France'.

    .. code-block:: PSQL

     OCTO> SELECT * FROM Customers
     OCTO> WHERE Country='France'
     OCTO> LIMIT 5;
     7|Blondel père et fils|Frédérique Citeaux|24, place Kléber|Strasbourg|67000|France
     9|Bon app'|Laurence Lebihans|12, rue des Bouchers|Marseille|13008|France
     18|Du monde entier|Janine Labrune|67, rue des Cinquante Otages|Nantes|44000|France
     23|Folies gourmandes|Martine Rancé|184, chaussée de Tournai|Lille|59000|France
     26|France restauration|Carine Schmitt|54, rue Royale|Nantes|44000|France

    The following query selects all products from the 'Products' table with a ProductName that starts with 'L'.

    .. code-block:: PSQL

     OCTO> SELECT * FROM Products
     OCTO> WHERE ProductName LIKE 'L%';
     65|Louisiana Fiery Hot Pepper Sauce|2|2|32 - 8 oz bottles|21.05
     66|Louisiana Hot Spiced Okra|2|2|24 - 8 oz jars|17
     67|Laughing Lumberjack Lager|16|1|24 - 12 oz bottles|14
     74|Longlife Tofu|4|7|5 kg pkg.|10
     76|Lakkalikööri|23|1|500 ml |18

    The following query displays the average price of Products per Category.

    .. code-block:: SQL

     OCTO> SELECT AVG(Price), CategoryID
     OCTO> FROM Products
     OCTO> GROUP BY CategoryID;
     37.9791666666666666|1
     23.0625|2
     25.16|3
     28.73|4
     20.25|5
     54.0066666666666666|6
     32.37|7
     20.6825|8

    The following query displays each Product with its Category and Supplier in ascending order of the 'SupplierName'.

    .. code-block:: PSQL

     OCTO> SELECT Products.ProductName, Categories.CategoryName, Suppliers.SupplierName
     OCTO> FROM ((Products
     OCTO> INNER JOIN Categories ON Products.CategoryID = Categories.CategoryID)
     OCTO> INNER JOIN Suppliers ON Products.SupplierID = Suppliers.SupplierID)
     OCTO> ORDER BY Suppliers.SupplierName;
     Côte de Blaye|Beverages|Aux joyeux ecclésiastiques
     Chartreuse verte|Beverages|Aux joyeux ecclésiastiques
     Sasquatch Ale|Beverages|Bigfoot Breweries
     Steeleye Stout|Beverages|Bigfoot Breweries
     Laughing Lumberjack Lager|Beverages|Bigfoot Breweries
     Queso Cabrales|Dairy Products|Cooperativa de Quesos 'Las Cabras'
     Queso Manchego La Pastora|Dairy Products|Cooperativa de Quesos 'Las Cabras'
     Escargots de Bourgogne|Seafood|Escargots Nouveaux
     Chais|Beverages|Exotic Liquid
     Chang|Beverages|Exotic Liquid
     Aniseed Syrup|Condiments|Exotic Liquid
     Gorgonzola Telino|Dairy Products|Formaggi Fortini s.r.l.
     Mascarpone Fabioli|Dairy Products|Formaggi Fortini s.r.l.
     Mozzarella di Giovanni|Dairy Products|Formaggi Fortini s.r.l.
     Sirop d'érable|Condiments|Forêts d'érables
     Tarte au sucre|Confections|Forêts d'érables
     Manjimup Dried Apples|Produce|G'day, Mate
     Filo Mix|Grains/Cereals|G'day, Mate
     Perth Pasties|Meat/Poultry|G'day, Mate
     Raclette Courdavault|Dairy Products|Gai pâturage
     Camembert Pierrot|Dairy Products|Gai pâturage
     Grandma's Boysenberry Spread|Condiments|Grandma Kelly's Homestead
     Uncle Bob's Organic Dried Pears|Produce|Grandma Kelly's Homestead
     Northwoods Cranberry Sauce|Condiments|Grandma Kelly's Homestead
     NuNuCa Nuß-Nougat-Creme|Confections|Heli Süßwaren GmbH & Co. KG
     Gumbär Gummibärchen|Confections|Heli Süßwaren GmbH & Co. KG
     Schoggi Schokolade|Confections|Heli Süßwaren GmbH & Co. KG
     Maxilaku|Confections|Karkki Oy
     Valkoinen suklaa|Confections|Karkki Oy
     Lakkalikööri|Beverages|Karkki Oy
     Singaporean Hokkien Fried Mee|Grains/Cereals|Leka Trading
     Ipoh Coffee|Beverages|Leka Trading
     Gula Malacca|Condiments|Leka Trading
     Rűgede sild|Seafood|Lyngbysild
     Spegesild|Seafood|Lyngbysild
     Tourtière|Meat/Poultry|Ma Maison
     Pâté chinois|Meat/Poultry|Ma Maison
     Konbu|Seafood|Mayumi's
     Tofu|Produce|Mayumi's
     Genen Shouyu|Condiments|Mayumi's
     Boston Crab Meat|Seafood|New England Seafood Cannery
     Jack's New England Clam Chowder|Seafood|New England Seafood Cannery
     Chef Anton's Cajun Seasoning|Condiments|New Orleans Cajun Delights
     Chef Anton's Gumbo Mix|Condiments|New Orleans Cajun Delights
     Louisiana Fiery Hot Pepper Sauce|Condiments|New Orleans Cajun Delights
     Louisiana Hot Spiced Okra|Condiments|New Orleans Cajun Delights
     Nord-Ost Matjeshering|Seafood|Nord-Ost-Fisch Handelsgesellschaft mbH
     Geitost|Dairy Products|Norske Meierier
     Gudbrandsdalsost|Dairy Products|Norske Meierier
     Flűtemysost|Dairy Products|Norske Meierier
     Gustaf's Knäckebröd|Grains/Cereals|PB Knäckebröd AB
     Tunnbröd|Grains/Cereals|PB Knäckebröd AB
     Gnocchi di nonna Alice|Grains/Cereals|Pasta Buttini s.r.l.
     Ravioli Angelo|Grains/Cereals|Pasta Buttini s.r.l.
     Pavlova|Confections|Pavlova, Ltd.
     Alice Mutton|Meat/Poultry|Pavlova, Ltd.
     Carnarvon Tigers|Seafood|Pavlova, Ltd.
     Vegie-spread|Condiments|Pavlova, Ltd.
     Outback Lager|Beverages|Pavlova, Ltd.
     Rössle Sauerkraut|Produce|Plutzer Lebensmittelgroßmärkte AG
     Thüringer Rostbratwurst|Meat/Poultry|Plutzer Lebensmittelgroßmärkte AG
     Wimmers gute Semmelknödel|Grains/Cereals|Plutzer Lebensmittelgroßmärkte AG
     Rhönbräu Klosterbier|Beverages|Plutzer Lebensmittelgroßmärkte AG
     Original Frankfurter grüne Soße|Condiments|Plutzer Lebensmittelgroßmärkte AG
     Guaraná Fantástica|Beverages|Refrescos Americanas LTDA
     Teatime Chocolate Biscuits|Confections|Specialty Biscuits, Ltd.
     Sir Rodney's Marmalade|Confections|Specialty Biscuits, Ltd.
     Sir Rodney's Scones|Confections|Specialty Biscuits, Ltd.
     Scottish Longbreads|Confections|Specialty Biscuits, Ltd.
     Inlagd Sill|Seafood|Svensk Sjöföda AB
     Gravad lax|Seafood|Svensk Sjöföda AB
     Röd Kaviar|Seafood|Svensk Sjöföda AB
     Mishi Kobe Niku|Meat/Poultry|Tokyo Traders
     Ikura|Seafood|Tokyo Traders
     Longlife Tofu|Produce|Tokyo Traders
     Zaanse koeken|Confections|Zaanse Snoepfabriek
     Chocolade|Confections|Zaanse Snoepfabriek


+++++++++++++++++++++++++++++++++
Test with dummy data using Rocto
+++++++++++++++++++++++++++++++++

  The :code:`northwind` data set can also be queried using Rocto (Remote Octo server).
  :code:`SQuirreL SQL` needs to be configured in order to use Rocto.
  An alias needs to be created, including the server IP address and port number.

  For example:

  .. code-block:: bash

     jdbc:postgresql://localhost:1337/

  A username and password should also be added to the alias.
  This username and password combination must first be added to Octo using the ydboctoAdmin utility:

  .. code-block:: bash

     yottadb -r %ydboctoAdmin add user <username>


  For example:

  .. code-block:: bash

     $ydb_dist/yottadb -r %ydboctoAdmin add user myusername
     Enter password for user myusername:
     Re-enter password for user myusername:
     Successfully added user: "myusername"


  In a shell with YottaDB and Octo environment variables set, start Rocto using the following command:

  .. code-block:: bash

     rocto

  Now, in SQuirreL SQL press the :code:`Connect` button for the alias created.
  You can now run queries on the :code:`northwind` data set through SQuirreL SQL.

  For example:

  .. figure:: squirrel.png

     Squirrel SQL Sample Screenshot

  Complete documentation of SQuirreL set-up can be found in the `ROcto Documentation <rocto.html>`_.

++++++
Usage
++++++

  Before running Octo/Rocto make sure that the required YottaDB variables are set either by creating your own script or run :code:`source $ydb_dist/ydb_env_set`.

  To use the command-line SQL interpreter run: :code:`$ydb_dist/plugin/bin/octo`.

  To use the PostgreSQL protocol compatible server run :code:`$ydb_dist/plugin/bin/rocto`.

  If you use the :code:`octo` command line interpreter, history is stored by
  default in :code:`~/.octo_history`. More information is provided in the
  :doc:`history` document.

++++++++++++++++++
Launching Options
++++++++++++++++++

  Octo has a few options that can be specified when it is launched.

.. _verbose-option:

~~~~~~~~~
  Verbose
~~~~~~~~~

    The verbose option specifies the amount of additional information that is provided to the user when commands are run in Octo.

    .. code-block:: bash

        --verbose={number}

    or equivalently,

    .. code-block:: bash

       -v{v{v}}

    The number given to the option corresponds to the following levels:

+-----------------+------------------------+---------------------------------------------+
| Number          | Level                  | Information                                 |
+=================+========================+=============================================+
| 0               | ERROR                  | Information about all errors                |
+-----------------+------------------------+---------------------------------------------+
| 1               | INFO                   | Additional information useful to log        |
+-----------------+------------------------+---------------------------------------------+
| 2               | DEBUG                  | Includes information useful for debugging   |
+-----------------+------------------------+---------------------------------------------+
| 3               | TRACE                  | Information logged stepping through actions |
+-----------------+------------------------+---------------------------------------------+

    When a number level is specified, the verbose output contains all information corresponding to that level as well as the previous levels.

    The default verbose level is set to zero(0) (ERROR).

    A single :code:`-v` in the command line puts the verbose level at one(1) (INFO), :code:`-vv` puts the level at two(2) (DEBUG), and :code:`-vvv` puts the level at three(3) (TRACE).

    Example:

    .. code-block:: bash

       octo --verbose=3

    Example:

    .. code-block:: bash

       OCTO> YDBOcto/build $ ./src/octo -vvv
       [TRACE] YDBOcto/src/octo.c:50 2019-04-10 10:17:57 : Octo started
       [ INFO] YDBOcto/src/run_query.c:79 2019-04-10 10:17:57 : Generating SQL for cursor 45
       [ INFO] YDBOcto/src/run_query.c:81 2019-04-10 10:17:57 : Parsing SQL command
       Starting parse
       Entering state 0
       Reading a token: OCTO> Next token is token ENDOFFILE (: )
       Shifting token ENDOFFILE (: )
       Entering state 15
       Reducing stack by rule 8 (line 182):
          $1 = token ENDOFFILE (: )
       Stack now 0
       [ INFO] YDBOcto/src/run_query.c:83 2019-04-10 10:18:00 : Done!
       [ INFO] YDBOcto/src/run_query.c:89 2019-04-10 10:18:00 : Returning failure from run_query

~~~~~~~~~
  Dry-run
~~~~~~~~~

    The dry-run option runs the parser, and performs checks and verifications on data types and syntax, but does not execute the SQL statements. The database is not altered when Octo is run with the :code:`--dry-run` option.

    .. code-block:: bash

       --dry-run

    or equivalently,

    .. code-block:: bash

       -d

    Example:

    .. code-block:: bash

       octo --dry-run

~~~~~~~~~
  Emulate
~~~~~~~~~

    The emulate option allows the user to specify which SQL database Octo should emulate. Database names should be in all caps. Currently supported emulations are MYSQL and POSTGRES.

    .. code-block:: bash

       --emulate=<db_name>

    or equivalently,

    .. code-block:: bash

       -e <db_name>

    Example:

    .. code-block:: bash

       octo --emulate=MYSQL

~~~~~~~~~~~~
  Input-file
~~~~~~~~~~~~

    The input-file option takes a file as input to Octo, that commands are then read from.

    .. code-block:: bash

       --input-file=<path to input file>

    or equivalently,

    .. code-block:: bash

       -f <input file>

    Example:

    .. code-block:: bash

       octo --input-file=files/commands.txt


+++++++++++++++++++++++++
Useful Commands at OCTO>
+++++++++++++++++++++++++

+-------------------------+-------------------------------------------------------+
| Command                 | Information                                           |
+=========================+=======================================================+
| :code:`\\q`             | Exits the prompt                                      |
+-------------------------+-------------------------------------------------------+
| :code:`\\d`             | Displays all relations                                |
+-------------------------+-------------------------------------------------------+
| :code:`\\d tablename`   | Displays information about columns of specified table |
+-------------------------+-------------------------------------------------------+
| :code:`\\s`             | Displays Octo command history                         |
+-------------------------+-------------------------------------------------------+

    Relation shown will be similar to the following:

    .. code-block:: bash

	OCTO> \d
	SCHEMA|NAME|TYPE|OWNER
	public|INFORMATION_SCHEMA.TABLES|table|octo
	public|NAMES|table|octo
	public|NAMESWITHAGES|table|octo
	public|OCTOONEROWTABLE|table|octo
	public|PG_ATTRDEF|table|octo
	public|PG_ATTRIBUTE|table|octo
	public|PG_CATALOG.PG_ATTRDEF|table|octo
	public|PG_CATALOG.PG_ATTRIBUTE|table|octo
	public|PG_CATALOG.PG_CLASS|table|octo
	public|PG_CATALOG.PG_DATABASE|table|octo
	public|PG_CATALOG.PG_DESCRIPTION|table|octo
	public|PG_CATALOG.PG_NAMESPACE|table|octo
	public|PG_CATALOG.PG_PROC|table|octo
	public|PG_CATALOG.PG_ROLES|table|octo
	public|PG_CATALOG.PG_SETTINGS|table|octo
	public|PG_CATALOG.PG_TYPE|table|octo
	public|PG_CATALOG.PG_USER|table|octo
	public|PG_CLASS|table|octo
	public|PG_DATABASE|table|octo
	public|PG_DESCRIPTION|table|octo
	public|PG_NAMESPACE|table|octo
	public|PG_PROC|table|octo
	public|PG_ROLES|table|octo
	public|PG_SETTINGS|table|octo
	public|PG_TYPE|table|octo
	public|PG_USER|table|octo
	(26 rows)

    .. code-block:: bash

       OCTO> \d NAMES
       Table "NAMES"
       Column|Type|Collation|Nullable|Default
       ID|INTEGER||NOT NULL|
       FIRSTNAME|VARCHAR(30)|||
       LASTNAME|VARCHAR(30)|||
       OCTO>
