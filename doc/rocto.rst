
=====================
Rocto
=====================

Rocto is the Remote Octo server. It is an implementation of the Postgres server, and recieves SQL queries from any clients that can communicate with Postgres over the network. These queries are passed to Octo, which interacts with the database and passes results back through Rocto to the clients.

---------------------------
Connecting to SquirrelSQL
---------------------------

`SquirreL SQL <http://squirrel-sql.sourceforge.net/>`_ is an open-source Java SQL Client program for any JDBC compliant database.

**To connect Rocto to Squirrel SQL**:

* Add a PostgreSQL `JAR <https://en.wikipedia.org/wiki/JAR_(file_format)>`_ file as a driver to SquirrelSQL.

* Configure the PostgreSQL driver to use the provided JAR file. The URL format for PostgreSQL in Squirrel SQL is :code:`jdbc:postgresql:[<//host>[:<5432>/]]<database>`.

.. figure:: driver.png
 
   Click "Add" and put in the path to the JAR file

Put in the path to your JAR file (e.g. :code:`~/user/postgresql-42.2.6-SNAPSHOT.jar`) in the dialog box that pops up after clicking "Add" on the interface above.  

* Create an alias for your Rocto server, including the server IP and port number. These can be configured for Rocto in the `config file <config.html#config-files>`_.
  For example,

  .. parsed-literal::
     jdbc:postgresql://localhost:1337/example

Add a user name and a password (if desired).

.. figure:: alias.png

* Rocto does not currently support SSL, so make sure to turn SSL off in the Properties menu when configuring an alias. (Click on Properties and then disable :code:`sslmode`.)

.. figure:: properties.png

* Connect to Rocto: Execute rocto in your terminal and press the "Connect" button in Squirrel SQL. Now you can execute SQL queries.

For example:

Suppose there is a table "names" with records in it:

.. parsed-literal::
   CREATE TABLE names (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR);

A simple query in Squirrel SQL could be:

.. parsed-literal::
   SELECT * FROM names;

.. figure:: query.png
 
   Result after the query
