
====================
Configuration
====================

.. contents::
   :depth: 5

Configuration settings can be passed to the program with the following precedence:

1. Flags passed to the program
2. Environment settings
3. .octo.conf
4. ~/.octo.conf
5. /etc/octo.conf

--------------------
Config files
--------------------

A config file contains settings related to YottaDB and the rocto process, and can also specify the level of verbosity for logging.

Sample config file:

.. literalinclude:: ../src/aux/octo.conf.default

~~~~~~~~~~~~~
Logging
~~~~~~~~~~~~~

A config file can include instructions specifying verbosity for logging:

* TRACE: A TRACE message is useful to help identify issues in the code
* INFO: An INFO message is used to relay information to the user
* DEBUG: A DEBUG message helps users debug configuration issues
* WARNING: A WARNING message warns the user about potential problems in the code
* ERROR : An ERROR message informs the user that an error has occurred
* FATAL: A FATAL message terminates the program

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Locations and Global Variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The config file includes the location of cache-generated M routines that represent queries, and the global directory to be used to source global variables for Octo.

The :code:`octo_global_prefix` can be set to a value that will then be prefixed to the M global variables used in Octo.

Example:

.. parsed-literal::
   octo_global_prefix = "%ydbocto"

The global variable :code:`^schema` will be :code:`^%ydboctoschema` as a global variable in Octo.

The global directory to be used for Octo can also be defined in the config file.

For example:

.. parsed-literal::
   octo_global_directory = "mumps.gld"

All globals should be preceded by :code:`^|<octo_global_directory>|<octo_global_prefix>`

Example:

.. parsed-literal::
   ^|mumps.gld|%ydboctoocto

Some of the globals used in Octo are:

* **octo**: This global can refer to various functions, variables, octo "read only" table values (postgres mappings, oneRowTable, etc.), and some counts. It needs to be journaled and persist between sessions.

* **session**: This global can contain session variables, portals and prepared queries. It need not be journaled/ persist between sessions since it only contains data related to the current session.

* **cursor**: This global contains output data and temporary tables. It need not be journaled/ persist between sessions since it only contains temporary data.

* **xref**: This global contains cross-references, row counts and other statistical information. It needs to be journaled and persist between sessions.

* **schema**: This global contains information about the tables loaded into the database. It needs to be journaled and persist between sessions. 


~~~~~~~~~~~~~~~~~~~~~~~
Environment Variables
~~~~~~~~~~~~~~~~~~~~~~~

The config file allows setting environment variables to be used at Octo startup.


