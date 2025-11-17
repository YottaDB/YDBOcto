.. #################################################################
.. #								   #
.. # Copyright (c) 2022-2025 YottaDB LLC and/or its subsidiaries.       #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

=======================
Developer Documentation
=======================

.. contents::
   :depth: 3

--------------------------------------------------
Setting up Automated Regression Testing for Octo
--------------------------------------------------

  .. note::

     Automated regression tests are run on every Octo source code update and install.
     Run BATS only if you are an advanced user who wants to contribute to Octo or run on a Linux distribution on which YottaDB is Supportable but not Supported.

+++++++++++++++++++
Installing YottaDB
+++++++++++++++++++

 Testing Octo requires YottaDB r1.34 or greater to be installed in UTF-8 mode, with AIM and POSIX plugins.

  .. code-block:: bash

     # Install prerequisite packages
     # Ubuntu/Debian
     apt update && apt install wget file procps libelf1 libicu-dev curl cmake make gcc pkg-config sudo git

     # Rocky Linux/RHEL
     yum install wget file procps-ng binutils findutils elfutils-libelf libicu libicu-devel curl cmake make gcc pkg-config sudo git nano gzip

     # SUSE (SLES or SLED) or OpenSUSE Leap or OpenSUSE Tumbleweed
     zypper install cmake make gcc git file curl tcsh binutils-gold icu {libconfig,libicu,ncurses,libelf,readline}-devel binutils ca-certificates

     # Install YottaDB
     mkdir /tmp/tmp ; wget -P /tmp/tmp https://gitlab.com/YottaDB/DB/YDB/raw/master/sr_unix/ydbinstall.sh
     cd /tmp/tmp ; chmod +x ydbinstall.sh
     ./ydbinstall.sh --overwrite-existing --utf8 --aim --posix

++++++++++++++++++++++++++++++++++++++++++++++++
Prerequisites for Automated Regression Testing
++++++++++++++++++++++++++++++++++++++++++++++++

~~~~~~~~~~~~~~
Ubuntu/Debian
~~~~~~~~~~~~~~

 Run the following commands to install all of the prerequisites needed for testing Octo on Ubuntu or Debian Linux. You can substitute `mariadb-server` and `mariadb-client` instead of `mysql-server` and `mysql-client` packages.

  .. code-block:: bash

    sudo apt-get install bats clang-format clang-tidy default-jdk expect golang-go locales libcmocka-dev postgresql-client postgresql mysql-client mysql-server unixodbc odbc-postgresql bison flex libreadline-dev libconfig-dev libssl-dev python3 ncat r-base r-base-dev libpq-dev libomp-dev libtirpc-dev
    locale-gen en_US.UTF-8

~~~~~~~~~~~~~~~~~
Rocky Linux/RHEL
~~~~~~~~~~~~~~~~~

 Run the following commands to install and setup all of the prerequisites needed for testing Octo on Rocky Linux or RHEL. You can substitute `mariadb` and `mariadb-server` for `mysql` and `mysql-server`.

  .. code-block:: bash

    # Rocky Linux
    sudo yum install epel-release  # make bats and R available next line; powertools below required for R dependencies
    sudo yum --enablerepo=powertools install bats R clang-tools-extra java-11-openjdk-devel expect golang glibc-langpack-en libcmocka-devel postgresql postgresql-server mysql mysql-server unixODBC postgresql-odbc bison flex readline-devel libconfig-devel openssl-devel python3 passwd nmap-ncat postgresql-devel libomp-devel libtirpc-devel

    # RHEL 8
    sudo subscription-manager repos --enable codeready-builder-for-rhel-8-x86_64-rpms
    sudo yum install epel-release  # make bats and R available next line
    sudo yum install bats R clang-tools-extra java-11-openjdk-devel expect golang glibc-langpack-en libcmocka-devel postgresql postgresql-server mysql mysql-server unixODBC postgresql-odbc bison flex readline-devel libconfig-devel openssl-devel python3 passwd nmap-ncat postgresql-devel libomp-devel libtirpc-devel

~~~~~~~~~~~~~~~~~
OpenSUSE/SLES
~~~~~~~~~~~~~~~~~

 Run the following commands to install and setup all of the prerequisites needed for testing Octo on OpenSUSE/SLES. You can substitute `mariadb` and `mariadb-server` for `mysql` and `mysql-server`.

  .. code-block:: bash

    # SLES
    zypper install bats clang java-11-openjdk-devel expect go glibc-langpack-en libcmocka-devel postgresql postgresql-server mysql mysql-server unixODBC psqlODBC bison flex readline-devel libconfig-devel libopenssl-devel python3 nmap-ncat R-base R-base-devel postgresql-devel libomp-devel libtirpc-devel

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Configure PostgreSQL and MySQL/MariaDB
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 PostgreSQL must be set up for the user who will be running the tests:

  .. code-block:: bash

     sudo -u postgres createuser $USER
     sudo -u postgres psql <<PSQL
     alter user $USER createdb;
     create database $USER LC_COLLATE='C' template=template0;
     PSQL

 MySQL/MariaDB must be set up for the user who will be running the tests, using a password of 'ydbrocks'. Assuming a :code:`bash` shell, the following can be run as a single command to do the necessary setup:

  .. code-block:: bash

     sudo mysql <<MYSQL
     CREATE USER '$USER'@'localhost' IDENTIFIED BY 'ydbrocks';
     GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES, RELOAD on *.* TO '$USER'@'localhost' WITH GRANT OPTION;
     FLUSH PRIVILEGES;
     MYSQL

+++++++++++++++
Download Octo
+++++++++++++++

 Clone the Octo source code repository in a temporary directory using the following commands:

  .. code-block:: bash

     mkdir tmp && cd tmp
     git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git
     cd YDBOcto
     mkdir build && cd build

++++++++++++++
Compile Octo
++++++++++++++

~~~~~~~~~~~~~
CMake Flags
~~~~~~~~~~~~~

 * Use :code:`DISABLE_INSTALL=ON` to disable the generation of installation rules for the :code:`make install` command.
 * Use :code:`FULL_TEST_SUITE=ON` to build the full test suite for Octo.
 * In addition, there is a speed test that can be enabled by :code:`TEST_SPEED=ON`. The speed test is intended for use in benchmarking and needs to be run separately from the full test suite. Run it with :code:`bats -T bats_tests/test_speed.bats` or with the equivalent command :code:`ctest -V -R test_speed`.

 A typical developer would use the following command:

  .. code-block:: bash

     cmake -D DISABLE_INSTALL=ON -D FULL_TEST_SUITE=ON ..

~~~~~~~~~
Compile
~~~~~~~~~

 Run the following command to compile Octo:

  .. code-block:: bash

     make -j `getconf _NPROCESSORS_ONLN`

~~~~~~~~~
Install
~~~~~~~~~

 For testing purposes, Octo installation is not necessary. However, the following command can be used to install Octo:

  .. code-block:: bash

     sudo -E make install

+++++++++++++++
Sanity Checks
+++++++++++++++

 Use the :ref:`Northwind <northwind-ddl-ex>` database to check if Octo has been setup properly. The dummy data set can be found in the :code:`tests/fixtures` subdirectory of the YDBOcto repository created by :code:`git clone https://gitlab.com/YottaDB/DBMS/YDBOcto.git`.

 Assuming that :code:`/tmp/YDBOcto` is the directory from the git clone :code:`https://gitlab.com/YottaDB/DBMS/YDBOcto.git` command:

  .. code-block:: bash

     # Set ydb_routines
     export ydb_routines=". src/utf8/_ydbocto.so"
     # Source ydb_* variables
     source /usr/local/etc/ydb_env_set
     # ydb_dir can optionally be set to use a location other than $HOME/.yottadb for the working environment.

     mupip load ../tests/fixtures/northwind.zwr
     src/octo -f ../tests/fixtures/northwind.sql

 Once loaded, start the Octo interactive shell and run the following SELECT command:

  .. code-block:: none

     src/octo
     OCTO> SELECT * FROM Suppliers;
     SUPPLIERID|SUPPLIERNAME|CONTACTNAME|ADDRESS|CITY|POSTALCODE|COUNTRY|PHONE
     1|Exotic Liquid|Charlotte Cooper|49 Gilbert St.|Londona|EC1 4SD|UK|(171) 555-2222
     2|New Orleans Cajun Delights|Shelley Burke|P.O. Box 78934|New Orleans|70117|USA|(100) 555-4822
     3|Grandma Kelly's Homestead|Regina Murphy|707 Oxford Rd.|Ann Arbor|48104|USA|(313) 555-5735
     4|Tokyo Traders|Yoshi Nagase|9-8 Sekimai Musashino-shi|Tokyo|100|Japan|(03) 3555-5011
     5|Cooperativa de Quesos 'Las Cabras'|Antonio del Valle Saavedra |Calle del Rosal 4|Oviedo|33007|Spain|(98) 598 76 54
     6|Mayumi's|Mayumi Ohno|92 Setsuko Chuo-ku|Osaka|545|Japan|(06) 431-7877
     7|Pavlova, Ltd.|Ian Devling|74 Rose St. Moonie Ponds|Melbourne|3058|Australia|(03) 444-2343
     8|Specialty Biscuits, Ltd.|Peter Wilson|29 King's Way|Manchester|M14 GSD|UK|(161) 555-4448
     9|PB Knäckebröd AB|Lars Peterson|Kaloadagatan 13|Göteborg|S-345 67|Sweden |031-987 65 43
     10|Refrescos Americanas LTDA|Carlos Diaz|Av. das Americanas 12.890|Săo Paulo|5442|Brazil|(11) 555 4640
     11|Heli Süßwaren GmbH & Co. KG|Petra Winkler|Tiergartenstraße 5|Berlin|10785|Germany|(010) 9984510
     12|Plutzer Lebensmittelgroßmärkte AG|Martin Bein|Bogenallee 51|Frankfurt|60439|Germany|(069) 992755
     13|Nord-Ost-Fisch Handelsgesellschaft mbH|Sven Petersen|Frahmredder 112a|Cuxhaven|27478|Germany|(04721) 8713
     14|Formaggi Fortini s.r.l.|Elio Rossi|Viale Dante, 75|Ravenna|48100|Italy|(0544) 60323
     15|Norske Meierier|Beate Vileid|Hatlevegen 5|Sandvika|1320|Norway|(0)2-953010
     16|Bigfoot Breweries|Cheryl Saylor|3400 - 8th Avenue Suite 210|Bend|97101|USA|(503) 555-9931
     17|Svensk Sjöföda AB|Michael Björn|Brovallavägen 231|Stockholm|S-123 45|Sweden|08-123 45 67
     18|Aux joyeux ecclésiastiques|Guylène Nodier|203, Rue des Francs-Bourgeois|Paris|75004|France|(1) 03.83.00.68
     19|New England Seafood Cannery|Robb Merchant|Order Processing Dept. 2100 Paul Revere Blvd.|Boston|02134|USA|(617) 555-3267
     20|Leka Trading|Chandra Leka|471 Serangoon Loop, Suite #402|Singapore|0512|Singapore|555-8787
     21|Lyngbysild|Niels Petersen|Lyngbysild Fiskebakken 10|Lyngby|2800|Denmark|43844108
     22|Zaanse Snoepfabriek|Dirk Luchte|Verkoop Rijnweg 22|Zaandam|9999 ZZ|Netherlands|(12345) 1212
     23|Karkki Oy|Anne Heikkonen|Valtakatu 12|Lappeenranta|53120|Finland|(953) 10956
     24|G'day, Mate|Wendy Mackenzie|170 Prince Edward Parade Hunter's Hill|Sydney|2042|Australia|(02) 555-5914
     25|Ma Maison|Jean-Guy Lauzon|2960 Rue St. Laurent|Montréal|H1J 1C3|Canada|(514) 555-9022
     26|Pasta Buttini s.r.l.|Giovanni Giudici|Via dei Gelsomini, 153|Salerno|84100|Italy|(089) 6547665
     27|Escargots Nouveaux|Marie Delamare|22, rue H. Voiron|Montceau|71300|France|85.57.00.07
     28|Gai pâturage|Eliane Noz|Bat. B 3, rue des Alpes|Annecy|74000|France|38.76.98.06
     29|Forêts d'érables|Chantal Goulet|148 rue Chasseur|Ste-Hyacinthe|J2S 7S8|Canada|(514) 555-2955
     (29 rows)
     OCTO>

 Run the following sample tests to check if test prerequisites have been satisfied:

  .. code-block:: bash

     bats bats_tests/hello_bats.bats
     bats bats_tests/hello_db.bats

+++++++++++++++++++++++
Running all the tests
+++++++++++++++++++++++

 To show the output of failed tests, export the environment variable :code:`CTEST_OUTPUT_ON_FAILURE=TRUE`. Alternatively, you can show output for only a single run by passing the argument to make, :code:`make CTEST_OUTPUT_ON_FAILURE=TRUE test`.

 For example, run the following command to run the full test suite:

  .. code-block:: bash

     ctest -j `getconf _NPROCESSORS_ONLN`
     ...
     100% tests passed, 0 tests failed out of 137

     Total Test time (real) = 1111.17 sec

 Bats tests directories for passed tests are deleted by default; if you wish to keep them, ``export octo_keep_bats_dirs=1``.

-------------
Contributing
-------------

 To contribute or help with further development, `fork <https://docs.gitlab.com/ee/user/project/repository/forking_workflow.html#create-a-fork>`_ the `YDBOcto repository <https://gitlab.com/YottaDB/DBMS/YDBOcto>`_, clone your fork to a local copy and begin contributing!

 Please also set up the pre-commit and pre-rebase script to automatically enforce some coding conventions. Creating a symbolic link to YDBOcto/pre-commit and YDBOcto/pre-rebase will be enough for the setup. Assuming you are in the top-level directory of your local copy, the following will work:

  .. code-block:: bash

     ln -s ../../pre-commit .git/hooks/pre-commit
     ln -s ../../pre-rebase .git/hooks/pre-rebase

 Note that this script requires :code:`tcsh` and :code:`clang-format` version 15 or later (installed above).

 The CI pipeline will also run the `clang-tidy <https://clang.llvm.org/extra/clang-tidy/>`_ tool version 8 or later (installed above), to catch common errors. You can replicate its behavior locally as follows:

  .. code-block:: bash

     mkdir build
     cd build
     cmake -D CMAKE_EXPORT_COMPILE_COMMANDS=ON ..
     clang-tidy ../src/octo_init.c  # replace octo_init.c with the file you want to check

+++++++++++
Dockerfiles
+++++++++++

 There are various Dockerfiles at the top of the source tree:

  - :code:`Dockerfile`
  - :code:`tools/misc-dockerfiles/Dockerfile-Tests.rocky`
  - :code:`tools/misc-dockerfiles/Dockerfile-Tests.ubuntu`
  - :code:`tools/misc-dockerfiles/Dockerfile-Tests.vista`

 :code:`Dockerfile` builds a docker container suitable for use for using Octo in a testing capacity. The other files are all testing related, and are used to replicate the Gitlab pipelines. There are instructions at the top of each file for usage as well as current limitations.
