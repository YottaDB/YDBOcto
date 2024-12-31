.. #################################################################
.. #								   #
.. # Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.  #
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

++++++++++++++++++++++++++++++++++++++
Creating a ROcto service using Systemd
++++++++++++++++++++++++++++++++++++++

ROcto can also be managed as a Systemd service by creating a ``rocto.service`` file in the appropriate directory, e.g. ``/lib/systemd/system/`` on Ubuntu. For example:

.. code-block::

    [Unit]
    Description=Rocto
    After=network.target

    [Service]
    Type=exec
    User=sam
    WorkingDirectory=/home/sam
    ExecStart=/bin/bash -c '. "$0" && exec "$@"' /usr/local/lib/yottadb/r138/pro/ydb_env_set /usr/local/lib/yottadb/r138/pro/plugin/bin/rocto -v -p 1337
    ExecStop=/usr/local/lib/yottadb/r138/pro/mupip stop ${MAINPID}

    [Install]
    WantedBy=multi-user.target

You can also use an environment file to set YottaDB environment variables. For instance, consider this ``rocto.service`` file:

.. code-block::

    [Unit]
    Description=Rocto
    After=network.target

    [Service]
    Type=exec
    User=sam
    WorkingDirectory=/extra3/vista/vehu/
    EnvironmentFile=/extra3/vista/vehu/etc/env-systemd
    ExecStartPre=rm -f /dev/shm/temp.dat
    ExecStartPre=/usr/local/lib/yottadb/r138/mupip create -reg=TEMP
    ExecStartPre=/usr/local/lib/yottadb/r138/mupip journal -recover -backward \"j/aim.mjl j/vehu.mjl j/octo.mjl\"
    ExecStart=/usr/local/lib/yottadb/r138/plugin/bin/rocto -v -p 1338
    ExecStop=/usr/local/lib/yottadb/r138/mupip stop ${MAINPID}

    [Install]
    WantedBy=multi-user.target

Here are the contents of the matching environment file, in this case ``/extra3/vista/vehu/etc/env-systemd``:

.. code-block::

    ydb_dist=/usr/local/lib/yottadb/r138
    ydb_tmp=/extra3/vista/vehu/tmp
    vista_home=/extra3/vista/vehu/
    ydb_linktmpdir=/extra3/vista/vehu/tmp
    ydb_gbldir=/extra3/vista/vehu/g/vehu.gld
    ydb_zinterrupt='I $$JOBEXAM^ZU($ZPOSITION)'
    ydb_lct_stdnull=1
    ydb_lvnullsubs=2
    ydb_zquit_anyway=1
    ydb_sysid=vehu
    ydb_zstep='n oldio s oldio=$i u 0 zp @$zpos b  u oldio'
    ydb_link=RECURSIVE
    ydb_xc_ydbposix=/usr/local/lib/yottadb/r138/plugin/ydbposix.xc
    ydb_routines=/extra3/vista/vehu/o*(/extra3/vista/vehu/r) $ydb_dist/plugin/o/_ydbposix.so $ydb_dist/plugin/o/_ydbocto.so $ydb_dist/plugin/o/_ydbaim.so $ydb_dist/plugin/o/_ydbgui.so $ydb_dist/plugin/o/_ydbmwebserver.so $ydb_dist/libyottadbutil.so


-------------------------------------
Writing queries using the YottaDB GUI
-------------------------------------

Octo queries can be written interactively using the YottaDB GUI. If you installed the GUI when you `installed YottaDB <https://yottadb.com/product/get-started/>`_, then you can start it by running:


.. code-block:: bash

    yottadb -run %ydbgui --readwrite

You can then access the GUI by directing your browser to ``localhost:9080``:

    .. figure:: images/gui-dashboard.jpg

    YottaDB GUI dashboard

To write a new query, navigate to Development -> Octo -> New Query:

    .. figure:: images/gui-new_query.jpg

    Navigating to the Octo query editor

Here, you can use the top text box to write SQL queries, then run them with the "play" button:

    .. figure:: images/gui-query_results.jpg

    Writing an Octo query in the GUI query editor

As seen in the above screenshot, you can also use the left hand tree to browse the SQL tables and functions. You can even also drag and drop tables and functions into the SQL editor.

-----------------------------------------
Accessing ROcto data on Microsoft Windows
-----------------------------------------

++++++++++++++++++++++++++++++++++++++
Configuring the PostgreSQL ODBC driver
++++++++++++++++++++++++++++++++++++++

The easiest way to access ROcto using Microsoft tools is via the PostgreSQL ODBC driver. So, before demonstrating how to use ROcto with Excel and PowerBI, we'll show you how to setup the PostgreSQL ODBC driver for use with ROcto on Windows.

First, download an ODBC driver with at least version 13 from the `PostgreSQL ODBC downloads page <https://www.postgresql.org/ftp/odbc/releases/>`_. Then, run the file you downloaded to open the install wizard, then click through to install the driver.

Once the ODBC driver is installed, run the ODBC Data Sources application (``odbcad32.exe``). In the application window, click ``Add ...`` to add a new User DSN:

    .. figure:: images/odbc-setup-1.png

    Running ODBC Data Sources

Then, in the ``Create New Data Source`` window, scroll down to the ``PostgreSQL ANSI`` driver you just installed, select it, and click ``Finish``:

    .. figure:: images/odbc-setup-2.png

    Creating a new Data Source

Next, in the ``PostgreSQL ANSI ODBC Driver (psqlODBC) Setup`` window, fill out the form with the connectivity information for your ROcto instance. For example, if accessing a ROcto instance running inside WSL2, your configuration will look similar to this:

    .. figure:: images/odbc-setup-3.png

    Configuring the PostgreSQL ANSI ODBC Driver

If you haven't yet started ROcto, then do so now. Then, click the ``Test`` button to test the connection; if everything is working, you should see a ``Connection successful`` message:

    .. figure:: images/odbc-setup-4.png

    Testing the PostgreSQL ANSI ODBC Driver connection

Press ``OK`` to go back to the main dialog.

    .. figure:: images/odbc-setup-5.png

    Successful PostgreSQL ANSI ODBC Driver connection

You are now done. Press ``OK`` to close the ODBC Data Sources application.

That's it for setting up the ODBC driver.

+++++++++++++++++++++++++++++++++++++++++
Importing ROcto data into Microsoft Excel
+++++++++++++++++++++++++++++++++++++++++

To import ROcto data into Microsoft Excel, start by opening up Excel. Then, navigate to the ``DATA`` tab and select ``From Other Sources``:

    .. figure:: images/rocto-excel-setup-1.png

    Getting data "From Other Sources" in Excel

In the drop down, select ``From Microsoft Query``:

    .. figure:: images/rocto-excel-setup-2.png

    Selecting "Microsoft Query" in Excel

Select the previously set-up Octo data source:

    .. figure:: images/rocto-excel-setup-8.png

    Choosing a ROcto data source in Excel

In the ``Query Wizard - Choose Columns`` window, select which tables and/or columns you would like to include in your query, e.g.:

    .. figure:: images/rocto-excel-setup-9.png

    Choosing data columns using the Excel Query Wizard

Then, click ``Next`` to move on, then click through the subsequent windows using the ``Next`` button until you get to the ``Query Wizard - Finish`` window. Then, click ``Finish``:

    .. figure:: images/rocto-excel-setup-10.png

    Finishing the Excel Query Wizard

Finally, in the ``Import Data`` window, specify how you'd like to view the data and where you want to put it, e.g.:

    .. figure:: images/rocto-excel-setup-11.png

    Importing ROcto data into Excel

You should then see the data you selected in your Excel spreadsheet, e.g.:

    .. figure:: images/rocto-excel-setup-12.png

    Viewing ROcto data imported into Excel

+++++++++++++++++++++++++++++++++++++++++++
Importing ROcto data into Microsoft PowerBI
+++++++++++++++++++++++++++++++++++++++++++

First, open PowerBI, then click on ``Get Data`` and select the ``More...`` option from the drop down menu:

    .. figure:: images/windows-powerbi-setup-1.png

    Bringing up the "Get Data"  menu in PowerBI

Next, search for ``ODBC`` in the ``Get Data`` window and select the ``ODBC`` option from the panel on the right, and then click ``Connect``:

    .. figure:: images/windows-powerbi-setup-2.png

    Choosing the ODBC driver data source in PowerBI

Then, select the Data Source Name of the Octo data source created in the above ODBC setup section and click ``OK``, e.g.:

    .. figure:: images/windows-powerbi-setup-3.png

    Choosing the ODBC Data Source Name in PowerBI

Next, put in your ROcto credentials and click the ``Connect`` button:

    .. figure:: images/windows-powerbi-setup-4.png

    Entering ROcto credentials in PowerBI

If everything works properly, you will then see the ``Navigator`` window, e.g.:

    .. figure:: images/windows-powerbi-setup-5.png

    Viewing the Navigator window in PowerBI

Here, you can navigate all the tables and columns in ROcto using the drill down menu on the left and select the ones you want to draw data from, e.g.:

    .. figure:: images/windows-powerbi-setup-6.png

    Using the Navigator window in PowerBI

Once you've chosen the data you want, you can then click ``Load`` to pull it into PowerBI:

    .. figure:: images/windows-powerbi-setup-7.png

    Loading data from the Navigator window in PowerBI

-----------------------------
Connecting using Java Clients
-----------------------------

Rocto supports multiple Java clients using the `Postgres JDBC driver
<https://jdbc.postgresql.org/>`_. The following clients are regularly tested to
confirm that they continue to work:

- `SQuirreL SQL <https://squirrel-sql.sourceforge.io/>`_
- `SQL Workbench/J <https://www.sql-workbench.eu/>`_
- `DBeaver Community <https://dbeaver.io/>`_

With each of these products, you have to pick the exact Postgres JDBC driver to use. The procedure is described in detail in "Connecting using SQuirreL SQL" below; it's similar for the other Java clients. Note that if the Java client gives you the option to use SQL transactions, you need to disable that as Octo does not yet support SQL transactions.

Not all JDBC versions listed at `JDBC driver website <https://jdbc.postgresql.org/>`_ work with Rocto. Here are the versions tested and whether they work or not:

  +----------------+---------------+---------------------------------+
  | Version        | Works?        | Notes                           |
  +================+===============+=================================+
  | 42.2.*         | Yes           |                                 |
  +----------------+---------------+---------------------------------+
  | 42.3.*         | No            | Requires SQL transactions which |
  |                |               | are not yet supported in Octo   |
  +----------------+---------------+---------------------------------+
  | 42.4.*         | Yes           |                                 |
  +----------------+---------------+---------------------------------+
  | 42.5.*         | Yes           |                                 |
  +----------------+---------------+---------------------------------+
  | 42.6.*         | Yes           |                                 |
  +----------------+---------------+---------------------------------+
  | 42.7.*         | Yes           | Currently tested in the         |
  |                |               | pipelines                       |
  +----------------+---------------+---------------------------------+

-----------------------------
Connecting using SQuirreL SQL
-----------------------------

++++++++++++
Introduction
++++++++++++

  `SQuirreL SQL <https://squirrel-sql.sourceforge.io/>`_ is an open-source Java SQL Client program for any JDBC compliant database. This documentation will describe in detail how to connect it to ROcto.

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

  Download `JAR <https://en.wikipedia.org/wiki/JAR_(file_format)>`_ from `Squirrel SQL's <https://squirrel-sql.sourceforge.io/#installation>`_ website.

  Launch the installer program as an administrator/root. On Windows, see `here <https://stackoverflow.com/questions/37105012/execute-jar-file-as-administrator-in-windows>`_ for some guidance, as it is not on the right-click menu for jars.

  Press Next three times going through these screens, NOTE where you are installing it, and STOP at the last one

  * Welcome Screen
  * Information Screen
  * Installation Path Screen (NOTE DOWN THE INSTALL PATH)
  * Extra Packs Screen (STOP HERE)

  At the Extra Packs Screen, scroll down and check "Optional Plugin - PostgreSQL", as shown in the following figure, then press Next.

    .. figure:: images/squirrel-install-extra-packs.png

    Check "Optional Plugin - PostgreSQL"

  Installation will proceed. Press Next after that to create shortcuts, then Next, then Done.

  Launch SquirrelSQL using the shortcut that got created on your desktop or menu.

  The first time you launch it, you will be greeted with a Welcome Screen. Go ahead and close that. This is what you should see now.

    .. figure:: images/squirrel-base-program.png

    SquirrelSQL before any configuration

  Click on Windows menu > View Drivers

    .. figure:: images/squirrel-view-drivers1.png

    SquirrelSQL View Drivers 1

  Scroll down until you see "PostgreSQL", and then click on it.

    .. figure:: images/squirrel-view-drivers2.png

    SquirrelSQL View Drivers 2

  Click on Drivers menu > Modify Driver

    .. figure:: images/squirrel-modify-postgres-driver1.png

    SquirrelSQL Modify Postgres Driver - Main Screen

  Click on the "Extra Class Path" tab

    .. figure:: images/squirrel-modify-postgres-driver2.png

    SquirrelSQL Modify Postgres Driver - Extra Path Tab

  Click on the "Add" button. A file open dialog will present itself.

    .. figure:: images/squirrel-modify-postgres-driver3.png

    SquirrelSQL Modify Postgres Driver - Add button dialog

  Remember the SquirrelSQL install path you were asked to note down? We need it now. Navigate to that install path, then to "plugins", then to "postgres", then to "lib".

    .. figure:: images/squirrel-modify-postgres-driver4.png

    SquirrelSQL Modify Postgres Driver - Navigation

  Within the "lib" directory, you will find two files. We want the one called "postgresql-nn.n.n.jar". Click on that.

    .. figure:: images/squirrel-modify-postgres-driver5.png

    SquirrelSQL Modify Postgres Driver - Select

  Press "open".

    .. figure:: images/squirrel-modify-postgres-driver6.png

    SquirrelSQL Modify Postgres Driver - After Open

  Next, press "List Drivers". You will see the "Class Name" fill out.

    .. figure:: images/squirrel-modify-postgres-driver7.png

    SquirrelSQL Modify Postgres Driver - List Drivers

  Then, you will see this after you press OK. Notice the check mark next to the driver. That's what we want to see.

    .. figure:: images/squirrel-modify-postgres-driver-done.png

    SquirrelSQL Modify Postgres Driver - Completion

  Next, create an alias for your ROcto server, including the server IP address and port number. First, click on the Windows menu > Aliases.

    .. figure:: images/squirrel-add-rocto-alias1.png

    SquirrelSQL Add Alias - Side Bar

  Then, click on Aliases, new Alias.

    .. figure:: images/squirrel-add-rocto-alias2.png

    SquirrelSQL Add Alias - Main Screen

  In here, fill the fields as follows:

    * Name: Any name will do. ROcto for now.
    * Driver: Should be pre-selected to PostgreSQL.
    * URL: Should be in the format :code:`jdbc:postgresql://{ip_address}:{port}/{db_name}`. Replace :code:`ip_address` and :code:`port` with proper values. Octo does not currently support multiple databases exposed from a single ROcto process, so :code:`db_name` can be anything.
    * Username: ROcto username set-up in pre-requisites section.
    * Password: ROcto password set-up in pre-requisites section.

  Here's a sample fully filled out dialog:

    .. figure:: images/squirrel-add-rocto-alias3.png

    SquirrelSQL Add Alias - Main Screen Filled Out

  You should press "Test" and then "Connect" on the Test Dialog to test your connection. Once you are done, press OK. Once you do that, you will be immediately presented with another dialog to connect to ROcto:

    .. figure:: images/squirrel-rocto-connect1.png

    SquirrelSQL ROcto Connection Prompt

  Now press "Connect". If you have a big schema, you will get this warning that it's taking too long to load. It's okay to ignore this warning. Press "Close".

    .. figure:: images/squirrel-rocto-connect-session-load-time-warning.png

    SquirrelSQL ROcto Load Time Warning

  At this point, you will see the main screen. In this screen, you can explore the schema for the tables in Octo.

    .. figure:: images/squirrel-rocto-connected-main-screen.png

    SquirrelSQL ROcto Connected At Last!

  To write SQL statements, switch to the SQL tab. Drag down the divider to give yourself more editing space.

    .. figure:: images/squirrel-rocto-connected-sql-tab.png

    SquirrelSQL ROcto SQL Tab

  Suppose there is a table "names" with records in it:

    .. code-block:: SQL

       CREATE TABLE names (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR);

  A simple query in SQuirreL SQL could be:

    .. code-block:: SQL

       SELECT * FROM names;

    .. figure:: images/query.png

    Result after the query

  To make querying easier, Octo supports "Auto-Complete". To initiate it, type :code:`TABLENAME.`, then press CTRL-SPACE. E.g.

    .. figure:: images/squirrel-rocto-autocomplete.png

    Octo Auto-Complete with Squirrel

-----------------
Connecting from R
-----------------
`R <https://www.r-project.org/>`_ is a free software environment for statistical computing and graphics. To connect R to Octo data, you can either use the JDBC driver or the Postgres driver.

The following are the steps for each one. Note that in the examples ROcto is listening at the localhost on port 1337 with user ydb with password ydbrocks.

Consult the `R Website <https://www.r-project.org/>`_ for specific install instructions for your platform. Type ``R`` to start R.

.. code-block:: R
   :caption: JDBC

        # Install and Use RJDBC package
        install.packages('RJDBC')
        library(RJDBC)

        drv <- JDBC("org.postgresql.Driver", /path/to/postgresJDBC.jar)

        # Connect to database
        conn <- dbConnect(drv, "jdbc:postgresql://localhost:1337/helloR", "ydb", "ydbrocks")

        # Load, summarize, create a pie chart into a pdf
        customers <- dbGetQuery(conn, "select * from nwcustomers")
        summary(customers)
        country_table <- table(customers$country)
        pdf('customers.pdf')
        pie(country_table)
        dev.off()

.. code-block:: R
   :caption: Postgres

        # Install and use RPostgres Package
        install.packages('RPostgres')
        library(DBI)

        # Connect to database
        con <- dbConnect(RPostgres::Postgres(), dbname = 'helloR', host = 'localhost',
                         port = 1337, user = 'ydb', password = 'ydbrocks')

        # Load, summarize, create a pie chart into a pdf
        query <- dbSendQuery(con, "SELECT * FROM nwcustomers")
        customers <- dbFetch(query)
        summary(customers)
        country_table <- table(customers$country)
        pdf('customers.pdf')
        pie(country_table)
        dev.off()

Sample output as an image (generated using the ``png()`` function):

    .. figure:: images/R-sample-output-chart.png
