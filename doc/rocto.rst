.. #################################################################
.. #								   #
.. # Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.  #
.. # All rights reserved.					   #
.. #								   #
.. #	This source code contains the intellectual property	   #
.. #	of its copyright holder(s), and is made available	   #
.. #	under a license.  If you do not know the terms of	   #
.. #	the license, please stop and do not read further.	   #
.. #								   #
.. #################################################################

=====================
ROcto
=====================

.. contents::
   :depth: 3

ROcto is the Remote Octo server. It is an implementation of the Postgres server, and receives SQL queries from any clients that can communicate with Postgres over the network. These queries are passed to Octo, which interacts with the database and passes results back through ROcto to the clients.

-----------
ROcto Usage
-----------

+++++++++++++++++++++++++
Starting & Stopping ROcto
+++++++++++++++++++++++++

  You can start ROcto by simply typing :code:`rocto`.

  To terminate the ROcto instance, you may use :code:`CTRL+C`. Note that the rocto instance may not exit immediately upon sending :code:`CTRL+C` if there are still active client connections. In that case, rocto will gracefully exit once all client connections are closed.

.. _rocto-cmd-flags:

++++++++++++++++++
Command Line Flags
++++++++++++++++++

  :code:`rocto` can take the following command line flags.

  .. note::

     Mandatory arguments for long options are also mandatory for short options.

  #. **-a,  \-\-allowschemachanges**

       .. note::

  	  The :code:`-a/--allowschemachanges` option is off by default and must be explicitly enabled since normal users will not need to change the schema. When using this option, implement security measures appropriate to the environment, e.g. network controls to restrict access to the listening TCP port to a specific host or subnet.

      Allows ROcto to make changes to the schema (CREATE TABLE and DROP TABLE) and to modify existing tables (INSERT, UPDATE, and DELETE).

  #. **-c,  \-\-config-file=<filepath>**

       .. note::

	  Octo looks for configuration files in default locations, e.g. :code:`$ydb_dist/plugin/octo/octo.conf`. If a configuration file is specified on the command line, this will override any configuration specified in files from the default locations.

      Use specified configuration file instead of the default.

  #. **-e,  \-\-emulate=<db_name>**

      Specify the SQL database that Octo should emulate. Currently supported emulations are MYSQL and POSTGRES. If you wish to emulate MariaDB, choose MYSQL.

  #. **-h,  \-\-help**

      Display the help message and exit.

  #. **-p,  \-\-port=<number>**

      Listen on the specified port.

  #. **-v,  \-\-verbose=<number>**

      Specify amount of information to output when running commands specifying a numeric value from zero to five or adding additional 'v' characters. See :ref:`Verbose Launching Option <verbose-option>` for more information on verbosity levels.

  #. **-r,  \-\-version**

      Display version information and exit.

  #. **-w,  \-\-readwrite**

      Allow users with read-write permissions to run INSERT, UPDATE, and DELETE.

  #. **-r,  \-\-release**

      Display release information and exit.

-----------------------------
Connecting using SQuirreL SQL
-----------------------------

++++++++++++
Introduction
++++++++++++

  `SQuirreL SQL <http://squirrel-sql.sourceforge.net/>`_ is an open-source Java SQL Client program for any JDBC compliant database. This documentation will describe in detail how to connect it to ROcto.

  Pre-requisite steps:

    1. Make sure you know the IP address/port for your ROcto server. The port can be configured for ROcto in the `config file <config.html#config-files>`_.
    2. ROcto requires the creation of a user name and password. Follow the directions given in the :doc:`Octo Administration Guide <admin>` to add a new user.

  Note that users must be created with adequate permissions to run certain types of SQL statements, i.e. CREATE, DROP, INSERT, UPDATE, and DELETE. See the :doc:`Octo Administration Guide <admin>` for more information on user permissions.

  The overall steps are as follows:

    1. Install Java on your computer.
    2. Install Squirrel SQL with the PostgreSQL driver
    3. Launch Squirrel SQL
    4. Configure the PostgreSQL driver to use the installed driver files.
    5. Create a connection (called an Alias) using the PostgreSQL driver.
    6. Connect using the Alias

  Keep in mind that an Alias in Squirrel really means a connection, or more properly, a type of connection using a specific driver.

++++++++++++++
Detailed Steps
++++++++++++++

  Java is required to be installed in order to use SQuirrel SQL. Install Java if not already installed. Note that the Oracle version has some licensing limitations, but OpenJDK does not; but the OpenJDK version does not include auto-updating capabilities on some platforms.

  Download `JAR <https://en.wikipedia.org/wiki/JAR_(file_format)>`_ from `Squirrel SQL's <http://squirrel-sql.sourceforge.net/#installation>`_ website.

  Launch the installer program as an administrator/root. On Windows, see `here <https://stackoverflow.com/questions/37105012/execute-jar-file-as-administrator-in-windows>`_ for some guidance, as it is not on the right-click menu for jars.

  Press Next three times going through these screens, NOTE where you are installing it, and STOP at the last one

  * Welcome Screen
  * Information Screen
  * Installation Path Screen (NOTE DOWN THE INSTALL PATH)
  * Extra Packs Screen (STOP HERE)

  At the Extra Packs Screen, scroll down and check "Optional Plugin - PostgreSQL", as shown in the following figure, then press Next.

    .. figure:: squirrel-install-extra-packs.png

    Check "Optional Plugin - PostgreSQL"

  Installation will proceed. Press Next after that to create shortcuts, then Next, then Done.

  Launch SquirrelSQL using the shortcut that got created on your desktop or menu.

  The first time you launch it, you will be greeted with a Welcome Screen. Go ahead and close that. This is what you should see now.

    .. figure:: squirrel-base-program.png

    SquirrelSQL before any configuration

  Click on Windows menu > View Drivers

    .. figure:: squirrel-view-drivers1.png

    SquirrelSQL View Drivers 1

  Scroll down until you see "PostgreSQL", and then click on it.

    .. figure:: squirrel-view-drivers2.png

    SquirrelSQL View Drivers 2

  Click on Drivers menu > Modify Driver

    .. figure:: squirrel-modify-postgres-driver1.png

    SquirrelSQL Modify Postgres Driver - Main Screen

  Click on the "Extra Class Path" tab

    .. figure:: squirrel-modify-postgres-driver2.png

    SquirrelSQL Modify Postgres Driver - Extra Path Tab

  Click on the "Add" button. A file open dialog will present itself.

    .. figure:: squirrel-modify-postgres-driver3.png

    SquirrelSQL Modify Postgres Driver - Add button dialog

  Remember the SquirrelSQL install path you were asked to note down? We need it now. Navigate to that install path, then to "plugins", then to "postgres", then to "lib".

    .. figure:: squirrel-modify-postgres-driver4.png

    SquirrelSQL Modify Postgres Driver - Navigation

  Within the "lib" directory, you will find two files. We want the one called "postgresql-nn.n.n.jar". Click on that.

    .. figure:: squirrel-modify-postgres-driver5.png

    SquirrelSQL Modify Postgres Driver - Select

  Press "open".

    .. figure:: squirrel-modify-postgres-driver6.png

    SquirrelSQL Modify Postgres Driver - After Open

  Next, press "List Drivers". You will see the "Class Name" fill out.

    .. figure:: squirrel-modify-postgres-driver7.png

    SquirrelSQL Modify Postgres Driver - List Drivers

  Then, you will see this after you press OK. Notice the check mark next to the driver. That's what we want to see.

    .. figure:: squirrel-modify-postgres-driver-done.png

    SquirrelSQL Modify Postgres Driver - Completion

  Next, create an alias for your ROcto server, including the server IP address and port number. First, click on the Windows menu > Aliases.

    .. figure:: squirrel-add-rocto-alias1.png

    SquirrelSQL Add Alias - Side Bar

  Then, click on Aliases, new Alias.

    .. figure:: squirrel-add-rocto-alias2.png

    SquirrelSQL Add Alias - Main Screen

  In here, fill the fields as follows:

    * Name: Any name will do. ROcto for now.
    * Driver: Should be pre-selected to PostgreSQL.
    * URL: Should be in the format :code:`jdbc:postgresql://{ip_address}:{port}/{db_name}`. Replace :code:`ip_address` and :code:`port` with proper values. Octo does not currently support multiple databases exposed from a single ROcto process, so :code:`db_name` can be anything.
    * Username: ROcto username set-up in pre-requisites section.
    * Password: ROcto password set-up in pre-requisites section.

  Here's a sample fully filled out dialog:

    .. figure:: squirrel-add-rocto-alias3.png

    SquirrelSQL Add Alias - Main Screen Filled Out

  You should press "Test" and then "Connect" on the Test Dialog to test your connection. Once you are done, press OK. Once you do that, you will be immediately presented with another dialog to connect to ROcto:

    .. figure:: squirrel-rocto-connect1.png

    SquirrelSQL ROcto Connection Prompt

  Now press "Connect". If you have a big schema, you will get this warning that it's taking too long to load. It's okay to ignore this warning. Press "Close".

    .. figure:: squirrel-rocto-connect-session-load-time-warning.png

    SquirrelSQL ROcto Load Time Warning

  At this point, you will see the main screen. In this screen, you can explore the schema for the tables in Octo.

    .. figure:: squirrel-rocto-connected-main-screen.png

    SquirrelSQL ROcto Connected At Last!

  To write SQL statements, switch to the SQL tab. Drag down the divider to give yourself more editing space.

    .. figure:: squirrel-rocto-connected-sql-tab.png

    SquirrelSQL ROcto SQL Tab

  Suppose there is a table "names" with records in it:

    .. code-block:: SQL

       CREATE TABLE names (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR);

  A simple query in SQuirreL SQL could be:

    .. code-block:: SQL

       SELECT * FROM names;

    .. figure:: query.png

    Result after the query

  To make querying easier, Octo supports "Auto-Complete". To initiate it, type :code:`TABLENAME.`, then press CTRL-SPACE. E.g.

    .. figure:: squirrel-rocto-autocomplete.png

    Octo Auto-Complete with Squirrel
