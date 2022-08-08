.. #################################################################
.. #								   #
.. # Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.       #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

==========================
Developer's Documentation
==========================

.. contents::
   :depth: 3

--------------------------------------------------
Setting up Automated Regression Testing for Octo
--------------------------------------------------

.. note::

   Automated regression tests are run on every Octo source code update and install.
   Run BATS only if you are an advanced user who wants to contribute to Octo or run on a Linux distribution on which YottaDB is Supportable but not Supported.
   
++++++++++++++++++++++++++++++++++++++++++++++++
Prerequisites for Automated Regression Testing
++++++++++++++++++++++++++++++++++++++++++++++++

1. Octo uses BATS for automated integration and regression testing. To use BATS to run tests on Octo, BATS version 1.1+ must be installed:

  .. code-block:: bash

     git clone https://github.com/bats-core/bats-core.git
     cd bats-core
     sudo ./install.sh /usr

  This will install BATS to /usr/bin. Note that installing to /usr may require root access or use of :code:`sudo`. To specify an alternative path change the argument to your preferred location, e.g. "/usr/local" to install to /usr/local/bin.

  Details available in the `BATS source repo <https://github.com/bats-core/bats-core>`_.

  Some bats tests also require go, java and expect.
  The appropriate libraries must installed:

  .. code-block:: bash

     # Ubuntu Linux/Debian Linux
     sudo apt-get install --no-install-recommends default-jdk expect golang-go

     # RHEL 8/Rocky Linux
     sudo yum install java-latest-openjdk expect golang

  Additionally, some tests require a JDBC driver. The JDBC driver must be downloaded to the build directory and JDBC_VERSION must be set in the environment. Versions starting with 42.2.6 are tested, but earlier versions may work. For example, using 42.2.12 version:

  .. code-block:: bash
		
     export JDBC_VERSION=42.2.12
     wget https://jdbc.postgresql.org/download/postgresql-$JDBC_VERSION.jar

2. *(Debian/Ubuntu only)* Install the en_US.utf8 locale

  Octo tests should be run with the en_US.utf8 locale due to collation order differences in other locales that cause some test outputs to not match reference outputs.

  .. code-block:: bash
		
     # Debian
     locale -a
     # if "en_US.utf8" does not appear among the available locales listed by the above command, proceed to the steps below:
     sudo vi /etc/locale.gen # or use your preferred text editor
     # Uncomment the line in /etc/locale.gen that reads "en_US.UTF-8 UTF-8", then save and exit
     sudo locale-gen

3. Install cmocka unit testing framework

  Octo uses cmocka for automated unit testing. To build and run Octo's unit tests, cmocka must be installed:

  .. code-block:: bash
		
     # Ubuntu Linux/Debian Linux
     sudo apt-get install --no-install-recommends libcmocka-dev

     # RHEL 8/Rocky Linux
     sudo yum install libcmocka-devel

4. Install PostgreSQL client (psql)

  Octo uses the psql PostgreSQL for some integration/regression tests. To build and run these tests, psql must be installed:

  .. code-block:: bash
		
     # Ubuntu Linux/Debian Linux
     sudo apt-get install --no-install-recommends postgresql-client

     # RHEL 8/Rocky Linux
     sudo yum install postgresql

5. Install PostgreSQL server

  Octo uses the PostgreSQL server for some integration/regression tests. To build and run these tests, PostgreSQL must be installed:

  .. code-block:: bash
		
     # Ubuntu Linux/Debian Linux
     sudo apt-get install --no-install-recommends postgresql

     # RHEL 8/Rocky Linux
     sudo yum install postgresql

  Additionally, PostgreSQL must be set up for the user who will be running the tests:

  .. code-block:: bash
		
     sudo -u postgres createuser [username]
     sudo -u postgres psql <<PSQL
     alter user [username] createdb;
     create database [username] LC_COLLATE='C' template=template0;
     PSQL

6. Install MySQL server and client (mysql)

  Octo uses the MySQL server for some integration/regression tests. To build and run these tests, MySQL must be installed:

  .. code-block:: bash
		
     # Ubuntu Linux/Debian Linux
     sudo apt-get install mysql-server mysql-client

     # RHEL 8/Rocky Linux
     yum install -y mysql-server

  Additionally, MySQL must be set up for the user who will be running the tests, using a password of 'ydbrocks'. Assuming a :code:`bash` shell, the following can be run as a single command to do the necessary setup:

  .. code-block:: bash
		
     sudo mysql <<MYSQL
     CREATE USER '$USER'@'localhost' IDENTIFIED BY 'ydbrocks';
     GRANT CREATE, ALTER, DROP, INSERT, UPDATE, DELETE, SELECT, REFERENCES, RELOAD on *.* TO '$USER'@'localhost' WITH GRANT OPTION;
     FLUSH PRIVILEGES;
     MYSQL

7. Install UnixODBC and the Postgres ODBC Shared Library

  Octo runs ODBC driver tests if the UnixODBC package is installed. To build and run these tests, you need to do the following:

  .. code-block:: bash
		
     # Ubuntu Linux/Debian Linux
     sudo apt-get install unixodbc odbc-postgresql

     # RHEL 8/Rocky Linux
     sudo yum install unixODBC postgresql-odbc

+++++++++++++++++++
Running the tests
+++++++++++++++++++

.. note::

   Make sure that `YottaDB <https://docs.yottadb.com/AdminOpsGuide/installydb.html#installing-yottadb>`_, :ref:`Octo <install-octo>`, and `YDBAIM <https://docs.yottadb.com/Plugins/ydbaim.html#installation>`_ are installed and up to date before running the tests.

To generate a Debug build instead of a Release build (the default), add :code:`-DCMAKE_BUILD_TYPE=Debug` to the CMake line.

To additionally disable the generation of installation rules for the :code:`make install`, add :code:`-DDISABLE_INSTALL=ON`. This can be useful when doing testing in a temporary build directory only.

To build the full test suite rather than a subset of it, the :code:`FULL_TEST_SUITE` option needs to be set to :code:`ON`, e.g. :code:`cmake -D FULL_TEST_SUITE=ON ..`. In addition, there is a speed test that can be enabled by :code:`cmake -D TEST_SPEED=ON`. The speed test is intended for use in benchmarking and needs to be run separately from the full test suite. Run it with :code:`bats -T bats_tests/test_speed.bats` or with the equivalent command :code:`ctest -V -R test_speed`.

To show the output of failed tests, export the environment variable :code:`CTEST_OUTPUT_ON_FAILURE=TRUE`. Alternatively, you can show output for only a single run by passing the argument to make: :code:`make CTEST_OUTPUT_ON_FAILURE=TRUE test`.

For example, run the following commands to build and test the full test suite:

.. code-block:: bash

   ydbuser@ydbdev:~/YDBOcto/build$ cmake -D FULL_TEST_SUITE=ON ..
   ...
   ydbuser@ydbdev:~/YDBOcto/build$ make -j `getconf _NPROCESSORS_ONLN`
   ...
   ydbuser@ydbdev:~/YDBOcto/build$ sudo make install
   ...
   ydbuser@ydbdev:~/YDBOcto/build$ ctest -j `getconf _NPROCESSORS_ONLN`
   ...
   100% tests passed, 0 tests failed out of 137

   Total Test time (real) = 1111.17 sec
   
-------------
Contributing
-------------

To contribute or help with further development, `fork <https://docs.gitlab.com/ee/gitlab-basics/fork-project.html>`_ the `YDBOcto repository <https://gitlab.com/YottaDB/DBMS/YDBOcto>`_, clone your fork to a local copy and begin contributing!

Please also set up the pre-commit script to automatically enforce some coding conventions. Creating a symbolic link to YDBOcto/pre-commit will be enough for the setup. Assuming you are in the top-level directory of your local copy, the following will work:

.. code-block:: bash

   ln -s ../../pre-commit .git/hooks

Note that this script will require :code:`tcsh` and :code:`clang-format-11` or a later release.

.. code-block:: bash

   # Ubuntu 20.04
   sudo apt install --no-install-recommends clang-format-11
   # Any Debian-like distro; see also https://apt.llvm.org/
   bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
   # RHEL 8/Rocky Linux
   sudo yum install clang-tools-extra

+++++++++++
clang-tidy
+++++++++++

The CI pipeline will run the `clang-tidy <https://clang.llvm.org/extra/clang-tidy/>`_ tool to catch common errors. You can replicate its behavior locally as follows:

.. code-block:: bash

   # Ubuntu 20.04
   sudo apt install --no-install-recommends clang-tidy
   # Any Debian-like distro
   bash -c "$(wget -O - https://apt.llvm.org/llvm.sh)"
   # RHEL 8/Rocky Linux
   sudo yum install clang-tools-extra

   mkdir build
   cd build
   cmake -D CMAKE_EXPORT_COMPILE_COMMANDS=ON ..
   clang-tidy ../src/octo_init.c  # replace octo_init.c with the file you want to check

:code:`clang-tidy-8` and later are supported.

+++++++++++
Dockerfiles
+++++++++++

There are 4 Dockerfiles at the top of the source tree:

- :code:`Dockerfile`
- :code:`Dockerfile-Tests.rocky`
- :code:`Dockerfile-Tests.ubuntu`
- :code:`Dockerfile-Tests.vista`

:code:`Dockerfile` builds a docker container suitable for use for using Octo in
a testing capacity. The other files are all testing related, and are used to
replicate the Gitlab pipelines. There are instructions at the top of each file
for usage as well as current limitations.
