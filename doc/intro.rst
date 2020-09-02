
====================
Introduction
====================

.. contents::
   :depth: 3

This manual documents Octo, the YottaDB Database Management System.

Octo is a layered application with a relational access model, built on top of the not-only-SQL database YottaDB. It aims to provide SQL 92 compliance and exceptional performance.

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

* The ability to define data structures, especially database schemas. (Data Definition Language, or DDL).
* The ability to retrieve data. (Data Query Language, or DQL).

 .. note::
    At the time of the release of this document, the features to manipulate data (Data Manipulation Language, or DML), manage transactions in the database (Transaction Control Language, or TCL), and control access to data stored in a database (Data Control Language, or DCL) are yet to be implemented.

--------------------
Setup
--------------------

YottaDB r1.28 or greater is required for successful installation of Octo.

Installing and configuring YottaDB is described in the `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/installydb.html>`__.

 .. note::
    It is required that the environment variable :code:`$ydb_dist` is defined - :code:`$gtm_dist` is not a valid subsitute.

-------------
Quickstart
-------------

~~~~~~~~~~~~~~~~~~~~~~
Install Prerequisites
~~~~~~~~~~~~~~~~~~~~~~

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Install YottaDB POSIX plugin
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  The YottaDB POSIX plugin can be installed easily by adding the :code:`--posix` option when installing YottaDB with the :code:`ydbinstall` script:

   .. code-block:: bash

      ./ydbinstall --posix

  Alternatively, users can build the POSIX plugin from source:

   .. code-block:: bash

      curl -fSsLO https://gitlab.com/YottaDB/Util/YDBPosix/-/archive/master/YDBPosix-master.tar.gz
      tar xzf YDBPosix-master.tar.gz
      cd YDBPosix-master
      mkdir build && cd build
      # Make sure that you have YottaDB environment variables in your shell before continuing
      cmake ..
      make -j `grep -c ^processor /proc/cpuinfo` && sudo make install

  More detailed instructions are on the `YottaDB POSIX plugin page <https://gitlab.com/YottaDB/Util/YDBPosix/blob/master/README.md/>`_.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  *(Optional)* Install YottaDB encryption plugin
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  Installing the YottaDB encryption plugin enables TLS support (Recommended for production installations). You will need to make sure TLS/SSL is enabled for the driver in the client software chosen.

  The YottDB encryption plugin can be installed by adding the :code:`--encplugin` option when installing YottaDB with the :code:`ydbinstall` script:

  .. code-block:: bash

     ./ydbinstall --encplugin

  Alternatively, users can build the encryption plugin from source:

  .. code-block:: bash

     # In a temporary directory perform the following commands
     sudo tar -xf $ydb_dist/plugin/gtmcrypt/source.tar
     # Make sure that you have YottaDB environment variables in your shell before continuing
     sudo ydb_dist=$ydb_dist make -j `grep -c ^processor /proc/cpuinfo`
     sudo ydb_dist=$ydb_dist make install

~~~~~~~~~~~~
Install Octo
~~~~~~~~~~~~

.. note::
   There are no binary releases during the beta period.

^^^^^^^^^^^^^^^
  From Tarball
^^^^^^^^^^^^^^^
  #. Decompress the Octo binary package

     .. code-block:: bash

	tar xzf YDBOcto-*-Linux.tar.gz

  #. Install Octo

     This will install Octo to your :code:`$ydb_dist/plugin` directory.

     .. code-block:: bash

	cd YDBOcto-*-Linux
	./install.sh

^^^^^^^^^^^^^^^
  From source
^^^^^^^^^^^^^^^

  .. note::

     This is the recommended instructions during the beta period as it provides the easiest upgrade path from each commit.

""""""""""""""""""""""""""""""""""
     Install Prerequisite Packages
""""""""""""""""""""""""""""""""""

     .. code-block:: bash

        # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
        sudo apt-get install build-essential cmake bison flex xxd libreadline-dev libssl-dev

        # CentOS Linux OR RedHat Linux
        # Note: epel-release has to be installed before cmake3 is installed
        sudo yum install epel-release
        sudo yum install cmake3 bison flex readline-devel vim-common libconfig-devel openssl-devel

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     *(Optional)* Install Bats Automated Test System (BATS)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

     Octo uses BATS for automated integration and regression testing. To use BATS to run tests on Octo, BATS version 1.1+ must be installed:

     .. code-block:: bash

	git clone https://github.com/bats-core/bats-core.git
	cd bats-core
	sudo ./install.sh /usr

     This will install BATS to /usr/bin. Note that installing to /usr may require root access or use of :code:`sudo`. To specify an alternative path change the argument to your preferred location, e.g. "/usr/local" to install to /usr/local/bin.


     Details available in the `BATS source repo <https://github.com/bats-core/bats-core>`_.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""
     *(Optional)* Install cmocka unit testing framework
"""""""""""""""""""""""""""""""""""""""""""""""""""""""

     Octo uses cmocka for automated unit testing. To build and run Octo's unit tests, cmocka must be installed:

     .. code-block:: bash

	# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	sudo apt-get install libcmocka-dev

	# CentOS Linux OR RedHat Linux
	sudo yum install libcmocka-devel

""""""""""""""""""""""""""""""""""""""""""""""""""
     *(Optional)* Install PostgreSQL client (psql)
""""""""""""""""""""""""""""""""""""""""""""""""""

     Octo uses the psql PostgreSQL for some integration/regression tests. To build and run these tests, psql must be installed:

     .. code-block:: bash

	# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	sudo apt-get install postgresql-client

	# CentOS Linux OR RedHat Linux
	sudo yum install postgresql

""""""""""""""""""""""""""""""""""""""""""""
     *(Optional)* Install PostgreSQL server
""""""""""""""""""""""""""""""""""""""""""""

     Octo uses the PostgreSQL server for some integration/regression tests. To build and run these tests, PostgreSQL must be installed:

     .. code-block:: bash

	# Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	sudo apt-get install postgresql

	# CentOS Linux OR RedHat Linux
	sudo yum install postgresql

     Additionally, PostgreSQL must be set up for the user who will be running the tests:

     .. code-block:: bash

	sudo -u postgres createuser [username]
	sudo -u postgres psql <<PSQL
	alter user [username] createdb;
	PSQL

""""""""""""""""""""""""""""""
     Download Octo Source Code
""""""""""""""""""""""""""""""

      .. code-block:: bash

         # In a temporary directory perform the following commands
         curl -fSsLO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/archive/master/YDBOcto-master.tar.gz
         tar xzf YDBOcto-master.tar.gz
         cd YDBOcto-master

"""""""""""""""""
     Compile Octo
"""""""""""""""""

      .. code-block:: bash

         mkdir build
         cd build
         # For VistA the String Buffer Length needs to be larger (described below) add "-DSTRING_BUFFER_LENGTH=300000" to the cmake command below
         cmake -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin .. # for CentOS/RedHat use cmake3 instead
         make -j `grep -c ^processor /proc/cpuinfo`

      To generate a Debug build instead of a Release build (the default), add :code:`-DCMAKE_BUILD_TYPE=Debug` to the CMake line above.

      To additionally disable the generation of installation rules for the :code:`make install`, add :code:`-DDISABLE_INSTALL=ON`. This can be useful when doing testing in a temporary build directory only.


      To build the full test suite rather than a subset of it, the :code:`FULL_TEST_SUITE` option needs to be set to :code:`ON`, e.g. :code:`cmake -D FULL_TEST_SUITE=ON ..`.

      .. note::

	 Octo uses some CMake parameters to control generation of fixed-size buffer allocations.


      These are:

         * :code:`STRING_BUFFER_LENGTH` -- The maximum length of a string within the system. Also, this supercedes any VARCHAR definitions.
         * :code:`INIT_M_ROUTINE_LENGTH` -- The initial length for the buffer of generated M routines. The default is 10MB.
         * :code:`MEMORY_CHUNK_SIZE` -- Size of memory chunks to allocate; default is 32MB.
         * :code:`MEMORY_CHUNK_PROTECT` -- If non-zero, memory following chunks is protected to detect buffer overflows. Set to 1 to detect buffer overflows and prevent then on mass-allocated memory chunks. Set to 2 to place data closer to the protected region to increase the chances of detecting an error.

      Example usage of the above parameters:

      .. code-block:: bash

         cmake -DSTRING_BUFFER_LENGTH=600000 -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin ..

"""""""""""""""""
     Install Octo
"""""""""""""""""

      Install Octo:

      .. code-block:: bash

         sudo -E make install

      Redefine environment variables to include newly installed files:

      .. code-block:: bash

   	 $ydb_dist/ydb_env_unset
 	 source $(pkg-config --variable=prefix yottadb)/ydb_env_set

      .. note::

	 New Octo installations include a default :code:`octo.conf` configuration file at :code:`$ydb_dist/plugin/octo/octo.conf`, which may be modified post-install. Re-installing Octo will *not* overwrite an existing :code:`octo.conf` in this location, so modifications to this file will be preserved across installations.

~~~~~~~~~~~~~~~~
Configure Octo
~~~~~~~~~~~~~~~~

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Setup environment variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  The following environment variables must be set for Octo to operate properly:

      * :code:`ydb_dist`
      * :code:`ydb_gbldir`
      * :code:`ydb_routines`
      * :code:`ydb_xc_ydbposix`

  The environment variables :code:`ydb_dist`, :code:`ydb_gbldir`, and :code:`ydb_routines` can initially be set by sourcing :code:`ydb_env_set` in your YottaDB installation directory. Additional modifications to ydb_routines may be needed due to configuration in :code:`octo.conf` described later in this manual.

  Example setting of the environment variables (assuming default paths):

  .. code-block:: bash

     source /usr/local/lib/yottadb/r1.28/ydb_env_set
     export ydb_routines="$ydb_dist/plugin/octo/o/_ydbocto.so $ydb_routines"
     export ydb_xc_ydbposix=$ydb_dist/plugin/ydbposix.xc

^^^^^^^^^^^^^^^^^^
  Setup Database
^^^^^^^^^^^^^^^^^^

  Octo uses several internal global variables to map a SQL schema/DDL to a YottaDB database: %ydboctoschema, %ydboctoxref, and %ydboctoocto. It is best practice to map these to a separate region that is exclusive to Octo, which requires settings that may conflict with those required by other regions. For more information, refer to the Additional Configuration section below.

  Please see the following example for creating a database from scratch with the recommended settings. For more information on setting up a database in YottaDB, refer to the `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/index.html>`__.

  .. code-block:: bash

     $ cd build
     $ export ydb_gbldir=*path to build directory*/octo.gld
     $ $ydb_dist/yottadb -run GDE
     GDE> add -segment OCTO -access_method=bg -file_name=*path to build directory*/octo.dat
     GDE> add -region OCTO -dynamic=octo -journal=(before,file="*path to build directory*/octo.mjl") -null_subscripts=always -key_size=1019 -record_size=300000
     GDE> add -name %ydboctoschema -region=octo
     GDE> add -name %ydboctoxref -region=octo
     GDE> add -name %ydboctoocto -region=octo
     GDE> verify
     GDE> exit
     $ mupip create

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Install PostgreSQL seed data
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  .. code-block:: bash

     $ydb_dist/mupip load $ydb_dist/plugin/octo/octo-seed.zwr
     $ydb_dist/plugin/bin/octo -f $ydb_dist/plugin/octo/octo-seed.sql

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Test with dummy data using Octo
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The :code:`northwind` dummy data set can be found in the :code:`tests/fixtures` directory of the YDBOcto repository. These are typically used for automated testing, but can also be used for manual testing.

  Each dummy data set consists of a :code:`.zwr` file and a :code:`.sql` file. The former contains the actual data to be stored in YottaDB, while the latter contains a schema that maps relational SQL structures (tables and columns) to the NoSQL data contained in YottaDB.

   .. note::

     The :code:`northwind` dummy data files are only available if Octo has built from source.
     If you are working with a binary distribution, download the required files first and then move on to loading them.

  Download :code:`northwind.zwr` and :code:`northwind.sql`:

  .. code-block:: bash

     # Create a new directory within Octo
     mkdir tests && cd tests
     # Download the required files
     curl -fSsLO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/blob/master/tests/fixtures/northwind.zwr
     curl -fSsLO https://gitlab.com/YottaDB/DBMS/YDBOcto/-/blob/master/tests/fixtures/northwind.sql

  Accordingly, to use this dummy data, both the data and DDL must be loaded.

  In a shell with no :code:`ydb*` environment variables defined other than :code:`ydb_dir`, do the following:

  .. code-block:: bash

     export ydb_chset="UTF-8"
     source $(pkg-config --variable=prefix yottadb)/ydb_env_set

  *(Optional)* If you would like to use a directory other than :code:`$HOME/.yottadb`, then set :code:`ydb_dir` to a directory of your choosing.

  For example:

  .. code-block:: bash

     export ydb_dir=/tmp/octodemo

  Now, load the northwind data set:

  .. code-block:: bash

     # In the /tests/fixtures directory

     $ydb_dist/mupip load northwind.zwr
     $ydb_dist/plugin/bin/octo -f northwind.sql

  Once loaded, these data sets may be queried with standard SQL queries.

^^^^^^^^^^^^^^^^^^^^^
Sample Queries
^^^^^^^^^^^^^^^^^^^^^

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

  .. code-block:: SQL

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Test with dummy data using Rocto
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~
Usage
~~~~~~~~~~~~~~~~~

Before running Octo/Rocto make sure that the required YottaDB variables are set either by creating your own script or run :code:`source $ydb_dist/ydb_env_set`.

To use the command-line SQL interpreter run: :code:`$ydb_dist/plugin/bin/octo`.

To use the PostgreSQL protocol compatible server run :code:`$ydb_dist/plugin/bin/rocto`.

~~~~~~~~~~~~~~~~~~~
Launching Options
~~~~~~~~~~~~~~~~~~~

Octo has a few options that can be specified when it is launched.

^^^^^^^^^^^
  Verbose
^^^^^^^^^^^

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

A single :code:`-v` in the command line puts the verbose level at 3, :code:`-vv` puts the level at 4, and :code:`-vvv` puts the level at 5.

Example:

.. code-block:: bash

   octo --verbose=4

Example:

.. code-block:: bash

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

^^^^^^^^^^^
  Dry-run
^^^^^^^^^^^

The dry-run option runs the parser, and performs checks and verifications on data types and syntax, but does not execute the SQL statements. The database is not altered when Octo is run with the :code:`--dry-run` option.

.. code-block:: bash

   --dry-run

or equivalently,

.. code-block:: bash

   -d

Example:

.. code-block:: bash

   octo --dry-run

^^^^^^^^^^^^^^
  Input-file
^^^^^^^^^^^^^^

The input-file option takes a file as input to Octo, that commands are then read from.

.. code-block:: bash

   --input-file=<path to input file>

or equivalently,

.. code-block:: bash

   -f <input file>

Example:

.. code-block:: bash

   octo --input-file=files/commands.txt

