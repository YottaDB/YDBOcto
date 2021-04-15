.. #################################################################
.. #								   #
.. # Copyright (c) 2018-2021 YottaDB LLC and/or its subsidiaries.  #
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

  * The ability to define data structures, especially database schemas. (Data Definition Language, or DDL).
  * The ability to retrieve data. (Data Query Language, or DQL).

  .. note::
    At the time of the release of this document, the features to manipulate data (Data Manipulation Language, or DML), manage transactions in the database (Transaction Control Language, or TCL), and control access to data stored in a database (Data Control Language, or DCL) are yet to be implemented.

--------------------
Setup
--------------------

  YottaDB r1.30 or greater is required for successful installation of Octo.

  Installing and configuring YottaDB is described on its own `documentation page <https://docs.yottadb.com/AdminOpsGuide/installydb.html>`__. With the :code:`--octo` and :code:`--posix` options of YottaDB's `ydbinstall.sh <https://gitlab.com/YottaDB/DB/YDB/-/blob/master/sr_unix/ydbinstall.sh>`_ script, you can install YottaDB and Octo with one command.

  .. note::
    Octo is a YottaDB application, not an application that runs on the upstream GT.M for which YottaDB is a drop-in upward-compatible replacement. Octo requires :code:`ydb*` environment variables to be defined, and does not recognize the :code:`gtm*` environment variables. Specifically, it requires :code:`ydb_dist` to be defined.

-------------
Quickstart
-------------

~~~~~~~~~~~~~~~~~~~~~~
Install Prerequisites
~~~~~~~~~~~~~~~~~~~~~~

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Install YottaDB POSIX plugin
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  The YottaDB POSIX plugin can be installed using the :code:`--posix` option when installing YottaDB with the :code:`ydbinstall` script:

   .. code-block:: bash

      ./ydbinstall --posix

  Alternatively, users can build the POSIX plugin from source:

   .. code-block:: bash

      #In a temporary directory perform the following commands
      git clone https://gitlab.com/YottaDB/Util/YDBPosix.git YDBPosix-master
      cd YDBPosix-master
      mkdir build && cd build
      # Make sure that you have YottaDB environment variables in your shell before continuing
      cmake ..
      make -j `grep -c ^processor /proc/cpuinfo` && sudo make install

  More detailed instructions are on the `YottaDB POSIX plugin page <https://gitlab.com/YottaDB/Util/YDBPosix>`_.

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

  Octo is a continuously updated YottaDB plugin that is distributed as source code. A CI (Continuous Integration) pipeline runs a considerable number of unit and system tests before allowing any source code to be merged. This ensures that the master branch is always current with the latest production-ready source code. Octo can be installed by using the :code:`--octo` option when installing YottaDB with the :code:`ydbinstall` script. Alternatively, you can build it from source.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     Install Prerequisite Packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

     .. code-block:: bash

        # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
        sudo apt-get install --no-install-recommends build-essential cmake bison flex xxd libreadline-dev libssl-dev

        # CentOS Linux OR RedHat Linux
        # Note: epel-release has to be installed before cmake3 is installed
        sudo yum install epel-release
        sudo yum install cmake3 bison flex readline-devel vim-common libconfig-devel openssl-devel

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   *(Optional)* Prerequisites for Automated Regression Testing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     .. note::

	As we run the automated regression tests on every Octo source code update, install and run BATS only if you are an advanced user who wants to contribute to Octo or run on a Linux distribution on which YottaDB is Supportable but not Supported.

     1. Octo uses BATS for automated integration and regression testing. To use BATS to run tests on Octo, BATS version 1.1+ must be installed:

        .. code-block:: bash

	   git clone https://github.com/bats-core/bats-core.git
	   cd bats-core
	   sudo ./install.sh /usr

        This will install BATS to /usr/bin. Note that installing to /usr may require root access or use of :code:`sudo`. To specify an alternative path change the argument to your preferred location, e.g. "/usr/local" to install to /usr/local/bin.


        Details available in the `BATS source repo <https://github.com/bats-core/bats-core>`_.

        Some bats tests also require go, java and expect. To run these, the appropriate libraries must be installed:

        .. code-block:: bash

	   # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	   sudo apt-get install --no-install-recommends default-jdk expect golang-go

	   #CentOS Linux or RedHat Linux
	   sudo yum install java-latest-openjdk expect golang

        Additionally, some tests requires a JDBC driver. The JDBC driver must be downloaded to the build directory and JDBC_VERSION must be set in the environment. Versions starting with 42.2.6 are tested, but earlier versions may work. For example, 42.2.12 is the latest release at the time of writing:

        .. code-block:: none

	   export JDBC_VERSION=42.2.12
	   wget https://jdbc.postgresql.org/download/postgresql-$JDBC_VERSION.jar

     2. *(Optional)* Install cmocka unit testing framework

        Octo uses cmocka for automated unit testing. To build and run Octo's unit tests, cmocka must be installed:

        .. code-block:: bash

	   # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	   sudo apt-get install --no-install-recommends libcmocka-dev

	   # CentOS Linux OR RedHat Linux
	   sudo yum install libcmocka-devel

     3. *(Optional)* Install PostgreSQL client (psql)

        Octo uses the psql PostgreSQL for some integration/regression tests. To build and run these tests, psql must be installed:

        .. code-block:: bash

	   # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	   sudo apt-get install --no-install-recommends postgresql-client

	   # CentOS Linux OR RedHat Linux
	   sudo yum install postgresql


     4. *(Optional)* Install PostgreSQL server

        Octo uses the PostgreSQL server for some integration/regression tests. To build and run these tests, PostgreSQL must be installed:

        .. code-block:: bash

	   # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	   sudo apt-get install --no-install-recommends postgresql

	   # CentOS Linux OR RedHat Linux
	   sudo yum install postgresql

        Additionally, PostgreSQL must be set up for the user who will be running the tests:

        .. code-block:: bash

	   sudo -u postgres createuser [username]
	   sudo -u postgres psql <<PSQL
	   alter user [username] createdb;
	   PSQL


     5. *(Optional)* Install UnixODBC and the Postgres ODBC Shared Library

        Octo runs ODBC driver tests if the UnixODBC package is installed. To build and run these tests, you need to do the following:

        .. code-block:: bash

	   # Ubuntu Linux OR Raspbian Linux OR Beagleboard Debian
	   sudo apt-get install unixodbc odbc-postgresql

	   # CentOS 8 Linux OR RedHat 8 Linux (names on 7 differ slightly)
	   sudo yum install unixODBC postgresql-odbc


     6. *(Optional - CentOS/RHEL7 only)* Install Perl

	On CentOS 7 and RHEL7, Octo test queries sometimes produce output with superfluous escape sequences. These escape sequences are removed by a Perl script, making Perl a dependency for Octo testing on these platforms.

	To install Perl on CentOS 7 or RHEL7:

	.. code-block:: bash

	   # CentOS Linux or RedHat Linux
	   sudo yum install perl

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     Clone the Octo source code repository
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      .. code-block:: bash

         # In a temporary directory perform the following commands
         git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git YDBOcto-master
         cd YDBOcto-master

^^^^^^^^^^^^^^^^^
     Compile Octo
^^^^^^^^^^^^^^^^^

      .. code-block:: bash

         mkdir build ; cd build
         cmake -DCMAKE_INSTALL_PREFIX=$ydb_dist/plugin .. # for CentOS/RedHat use cmake3 instead
         make -j `grep -c ^processor /proc/cpuinfo`

      Most users proceed to the *Install Octo* step below. The instructions here are for those wishing to contribute to Octo, or building it on Supportable but not Supported platforms.

      To generate a Debug build instead of a Release build (the default), add :code:`-DCMAKE_BUILD_TYPE=Debug` to the CMake line above.

      To additionally disable the generation of installation rules for the :code:`make install`, add :code:`-DDISABLE_INSTALL=ON`. This can be useful when doing testing in a temporary build directory only.


      To build the full test suite rather than a subset of it, the :code:`FULL_TEST_SUITE` option needs to be set to :code:`ON`, e.g. :code:`cmake -D FULL_TEST_SUITE=ON ..`. In addition, there is a speed test that can be enabled by :code:`cmake -D TEST_SPEED=ON`. The speed test is intended for use in benchmarking and needs to be run separately from the full test suite. Run it with :code:`bats -T bats_tests/test_speed.bats` or with the equivalent command :code:`ctest -V -R test_speed`.

      To show the output of failed tests, export the environment variable :code:`CTEST_OUTPUT_ON_FAILURE=TRUE`. Alternatively, you can show output for only a single run by passing the argument to make: :code:`make CTEST_OUTPUT_ON_FAILURE=TRUE test`.

^^^^^^^^^^^^^^^^^
     Install Octo
^^^^^^^^^^^^^^^^^

      Install Octo:

      .. code-block:: bash

         sudo -E make install

      Redefine environment variables to include newly installed files:

      .. code-block:: bash

   	 source $ydb_dist/ydb_env_unset
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

  Octo uses several global variables for its operation, all of which start with :code:`%ydbocto`. Use `GDE <https://docs.yottadb.net/AdminOpsGuide/gde.html>`_ to map :code:`%ydbocto*` global variables to a separate region. Global variables used by Octo must have `NULL_SUBSCRIPTS=ALWAYS <https://docs.yottadb.net/AdminOpsGuide/gde.html#no-n-ull-ubscripts-always-never-existing>`_.

  The following example creates an OCTO database region with the recommended setting in the :code:`$ydb_dir/$ydb_rel/g` directory and assumes an existing application global directory at :code:`$ydb_dir/$ydb_rel/g/yottadb.gld`. For more information on setting up a database in YottaDB, refer to the `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/index.html>`_, and the `YottaDB Acculturation Guide <https://docs.yottadb.com/AcculturationGuide/>`_ for self-paced exercises on YottaDB DevOps.

  .. code-block:: bash

     $ echo $ydb_dir $ydb_rel
     /tmp/test r1.30_x86_64
     $ yottadb -run GDE
     %GDE-I-LOADGD, Loading Global Directory file
             /tmp/test/r1.30_x86_64/g/yottadb.gld
     %GDE-I-VERIFY, Verification OK


     GDE> add -segment OCTO -access_method=BG -file_name=$ydb_dir/$ydb_rel/g/octo.dat
     GDE> add -region OCTO -dynamic=OCTO -null_subscripts=ALWAYS -key_size=1019 -record_size=300000 -journal=(before,file="$ydb_dir/$ydb_rel/g/octo.mjl")
     GDE> add -name %ydbocto* -region=OCTO
     GDE> verify
     %GDE-I-VERIFY, Verification OK


     GDE> exit
     %GDE-I-VERIFY, Verification OK

     %GDE-I-GDUPDATE, Updating Global Directory file
             /tmp/test/r1.30_x86_64/g/yottadb.gld
     $ mupip create -region=OCTO
     %YDB-I-DBFILECREATED, Database file /tmp/test/r1.30_x86_64/g/octo.dat created
     $ mupip set -journal=before,enable,on -region OCTO
     %YDB-I-JNLCREATE, Journal file /tmp/test/r1.30_x86_64/g/octo.mjl created for region OCTO with BEFORE_IMAGES
     %YDB-I-JNLSTATE, Journaling state for region OCTO is now ON
     $

  The commands in the example above are reproduced below, to facilitate copying and pasting.

  .. code-block:: bash

     echo $ydb_dir $ydb_rel
     yottadb -run GDE
     add -segment OCTO -access_method=BG -file_name=$ydb_dir/$ydb_rel/g/octo.dat
     add -region OCTO -dynamic=OCTO -null_subscripts=ALWAYS -key_size=1019 -record_size=300000 -journal=(before,file="$ydb_dir/$ydb_rel/g/octo.mjl")
     add -name %ydbocto* -region=OCTO
     verify
     exit
     mupip create -region=OCTO
     mupip set -journal=before,enable,on -region OCTO

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  *(Optional)* Test with dummy data using Octo
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  You can use the `Northwind <https://docs.yottadb.com/Octo/grammar.html#northwind-ddl-example>`_ sample database to get started. The dummy data set can be found in the :code:`tests/fixtures` subdirectory of the YDBOcto repository created by :code:`git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git YDBOcto-master`.

  A dummy data set consists of a :code:`.zwr` file and a :code:`.sql` file. The former contains the actual data to be stored in YottaDB, while the latter contains a schema that maps relational SQL structures (tables and columns) to the NoSQL data contained in YottaDB. Assuming that :code:`/tmp/YDBOcto-master` is the directory from the :code:`git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git YDBOcto-master` command :

   .. code-block:: bash

      # Unset all ydb_*, gtm* and GTM* environment variables:
      unset `env | grep -Ei ^\(\(gtm\)\|\(ydb\)\) | cut -d= -f 1`
      export ydb_chset=UTF-8
      # Source ydb_* variables:
      source $(pkg-config --variable=prefix yottadb)/ydb_env_set
      # ydb_dir can optionally be set to use a location other than $HOME/.yottadb for the working environment.

      mupip load /tmp/YDBOcto-master/build/tests/fixtures/northwind.zwr
      octo -f /tmp/YDBOcto-master/build/tests/fixtures/northwind.sql

  Once loaded, you can run `octo` to start the Octo interactive shell and use `SELECT <https://docs.yottadb.com/Octo/grammar.html#select>`_ queries to access the data.

""""""""""""""""""
Sample Queries
""""""""""""""""""

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


~~~~~~~~~~~~~~~~~~~~~~~~~
Use a Docker Container
~~~~~~~~~~~~~~~~~~~~~~~~~

  A Docker image is available on `docker hub <https://hub.docker.com/r/yottadb/octo>`_. This image is built with the following assumptions about the host environment and automatically starts :code:`rocto` when run by Docker using the commands below.

  * The :code:`ydb_env_set` script is used to setup the YottaDB environment and creates/expects a specific layout for globals and routines, specifically:

    * a :code:`r1.30_x86_64` directory with the following sub directories:

      * :code:`g` directory which contains at a minimum:

	* :code:`yottadb.gld` global directory

      * :code:`o` directory which contains the compiled M code
      * :code:`r` directory which contains the source M code
    * a :code:`r` directory which contains the source M code
  * The octo default configuration is used in :code:`/opt/yottadb/current/plugin/octo/octo.conf`

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Starting the Docker Container
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    To start the Docker container and make rocto available on the host's network on the default port 1337 (unless octo.conf within the container is configured otherwise):

    .. code-block:: bash

       docker run -it --network=host yottadb/octo:latest-master

    To login with the default :code:`ydb` user use :code:`psql` and enter :code:`ydbrocks` when prompted for a password:

    .. code-block:: bash

       psql -U ydb -h localhost -p 1337

    If you would like to use YDB data in an existing local directory structure, then issue the :code:`docker run` command from a directory where the above directory structure is defined. This is needed to mount it as a volume within the Docker container.

    .. code-block:: bash

       docker run -it -v `pwd`:/data yottadb/octo:latest-master

    This will then display the rocto log file on stdout. If you would prefer to run the container as a daemon use the :code:`-d` command line parameter. Also, if you would like to publish the container on specific ports, specify this with the :code:`-p` option. For example:

    .. code-block:: bash

       docker run -itd -v `pwd`:/data -p 1337:1337 yottadb/octo:latest-master

    The logs can then be retrieved using the :code:`docker logs` command with the container name or ID as an argument.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Getting access to the container
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

""""""""""""""""""""""""""
PostgreSQL wire protocol
""""""""""""""""""""""""""
    The rocto server is listening on port 1337 and all of the directions in the above documentation apply.

"""""""""""""""""""""
Command-line access
"""""""""""""""""""""

    You can use the :code:`docker exec` command to get access to the container for more troubleshooting. Example:

    .. code-block:: bash

       docker exec -it {nameOfContainer/IDOfContainer} /bin/bash

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

     Squirrel SQL Sample Screenshot

  Complete documentation of SQuirreL set-up can be found in the `ROcto Documentation <rocto.html>`_.

~~~~~~~~~~~~~~~~~
Usage
~~~~~~~~~~~~~~~~~

  Before running Octo/Rocto make sure that the required YottaDB variables are set either by creating your own script or run :code:`source $ydb_dist/ydb_env_set`.

  To use the command-line SQL interpreter run: :code:`$ydb_dist/plugin/bin/octo`.

  To use the PostgreSQL protocol compatible server run :code:`$ydb_dist/plugin/bin/rocto`.

  If you use the :code:`octo` command line interpreter, history is stored by
  default in :code:`~/.octo_history`. More information is provided in the
  :doc:`history` document.

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

    The default verbose level is set to two(2) (WARNING).

    A single :code:`-v` in the command line puts the verbose level at three(3), :code:`-vv` puts the level at four(4), and :code:`-vvv` puts the level at five(5).

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

