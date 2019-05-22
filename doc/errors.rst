
==========================
Error Messages
==========================

-------------------------
Error Message Severities
-------------------------

The error message severities are:

* TRACE
* INFO
* DEBUG 
* WARNING
* ERROR
* FATAL

Comparative to severities in PostgreSQL,

* TRACE, INFO and WARNING are equivalent to PSQL INFO.
* DEBUG is equivalent to PSQL DEBUG.
* ERROR is equivalent to PSQL ERROR.
* FATAL is equivalent to PSQL FATAL.

TRACE, INFO and DEBUG are used to get helpful output for debugging. WARNING indicates strange behavior, invalid input or bad configuration. ERROR messages cause the currently executing query to fail. FATAL messages cause the program to halt and produce a core file.

---------------
Octo Errors
---------------

Errors in Octo are of the form :code:`ERR_<error>`. The errors are detailed below, in alphabetical order.

.. contents::
   :local:

+++++++++++++++++++++
BAD_ADDRESS
+++++++++++++++++++++

This error is generated because of a bad listen address.

+++++++++++++++++++++
BAD_CONFIG
+++++++++++++++++++++

This error is generated because of a bad config setting.

+++++++++++++++++++++
BUFFER_TOO_SMALL
+++++++++++++++++++++

The buffer used to store output plans is too small for the query.

++++++++++++++++++++++
CLIENT_CONNECTED
++++++++++++++++++++++

This error lets the user know that the client is connected.

+++++++++++++++++++++
CURPLAN
+++++++++++++++++++++

This message is a text representation of the current logical plan.

++++++++++++++++++++++
DATABASE_FILES_OOS
++++++++++++++++++++++

This error means that the generated routines and the database seem to be out of sync.

+++++++++++++++++++++++++
ENTERING_FUNCTION
+++++++++++++++++++++++++

This message is generated when a function is entered.

++++++++++++++++++++++
FAILED_TO_PARSE_SCHEMA
++++++++++++++++++++++

This error is generated when a table failed to parse a schema.

++++++++++++++++++++++++++++
FEATURE_NOT_IMPLEMENTED
++++++++++++++++++++++++++++

A user attempted to use a feature that has not been implemented yet, generating an error.

+++++++++++++++++++++
FILE_NOT_FOUND
+++++++++++++++++++++

Octo encountered an error when trying to open a file.

++++++++++++++++++++++++++++
GENERATING_TEMPORARY_TABLE
++++++++++++++++++++++++++++

Octo encountered this error when a temporary table was generated.

+++++++++++++++++
INIT_SCANNER
+++++++++++++++++

There was an error intializing the scanner used to parse provided input. Please contact your YottaDB support channel.

+++++++++++++++++++++
INVALID_READ_SIZE
+++++++++++++++++++++

This message indicates that an attempt to read a PSQL message from the wire failed because the provided size was too small. 
This likely represents an error with the client attempting to connect to rocto. Please contact your YottaDB support channel.

+++++++++++++++++++++++
INVALID_WRITE_SIZE
+++++++++++++++++++++++

This indicates an issue writing a PSQL message to the wire. Please contact your YottaDB support channel.

++++++++++++++++++
LEAVING_FUNCTION
++++++++++++++++++

This message is generated when the flow of control is leaving a function and is valuable to Octo developers to debug behavior.

++++++++++++++++++++
LIBSSL_ERROR
++++++++++++++++++++

This error is generated when there is a problem with libssl/libcrypt.

++++++++++++++++++
LINE_TOO_LONG
++++++++++++++++++

This error is generated if the input line is too long.

++++++++++++++++++
LOADING_CONFIG
++++++++++++++++++

This message gives information about where the config is loaded from.

++++++++++++++++++++
MISSING_KEY
++++++++++++++++++++

This error indicates that the schema for table dddd did not have the correct number of keys. To solve this issue, the schema needs to be corrected.

++++++++++++++++++++++++++++
MULTIPLE_ZERO_KEYS
++++++++++++++++++++++++++++

This message indicates that the table has multiple KEY NUM elements with the same number, and that the source schema needs to be corrected.

+++++++++++++++++++
PARSING_COMMAND
+++++++++++++++++++

This message means that the statement or command given was not able to be parsed.

+++++++++++++++++++++
PARSING_CONFIG
+++++++++++++++++++++

This error is generated when there is an error parsing the config.

++++++++++++++++++++++
PLAN_HASH_FAILED
++++++++++++++++++++++

This error is generated when Octo failed to generate the filename hash for the plan.

++++++++++++++++++++++
PLAN_NOT_WELL_FORMED
++++++++++++++++++++++

This error is generated when the plan produced by the optimizer does not seem correct. Please contact your YottaDB support channel.

++++++++++++++++++++++
PRIMARY_KEY_NOT_FOUND
++++++++++++++++++++++

This error is generated when a table was created without specifying a primary key.

++++++++++++++++++++++++
PROCESSING_MESSAGE_TYPE
++++++++++++++++++++++++

This is a debug message useful when debugging the rocto server PSQL protocol implementation and indicates that a message of a particular type is being processed.

++++++++++++++++++++++
PSQL_STARTUP_PAIR
++++++++++++++++++++++

This message is used to inform the rocto server about the session variables being passed from the client to the server during initialization.

++++++++++++++++++++
READ_MESSAGE
++++++++++++++++++++

This is a debug message useful when debugging the rocto server PSQL protocol implementation and indicates that a message of the specified format was read from the wire.

++++++++++++++++++++
READ_TOO_LARGE
++++++++++++++++++++

This error indicates that a PSQL wire protocol message exceeded the maximum size of messages which can be read by rocto. Please contact your YottaDB support channel.

++++++++++++++++++
SEND_MESSAGE
++++++++++++++++++

This is a debug message useful when debugging the rocto server PSQL protocol implementation and indicates that a message of the specified format was written to the wire.

+++++++++++++++++
SYSCALL
+++++++++++++++++

This error is generated when a system call failed.

++++++++++++++++++++++++++
TABLE_DEFINITION_TOO_LONG
++++++++++++++++++++++++++

This error is generated when the table definition is too long.

++++++++++++++++++++
TABLE_NOT_FOUND
++++++++++++++++++++

This message means that the referenced table was not found in the database.

+++++++++++++++++++
TYPE_MISMATCH
+++++++++++++++++++

This error is generated when there is a type mismatch between parameters.

+++++++++++++++++++++++
UNKNOWN_COLUMN_NAME
+++++++++++++++++++++++

This error is generated when a column referenced does not exist or is unknown.

++++++++++++++++++++++++++
UNKNOWN_KEYWORD_STATE
++++++++++++++++++++++++++

An unknown state was reached, which generated an error. Please contact your YottaDB support channel.

+++++++++++++++++++++++++
UNKNOWN_MESSAGE_TYPE
+++++++++++++++++++++++++

This error is generated when an unknown message type was received from the front end. Please contact your YottaDB support channel.

+++++++++++++++++++++
UNKNOWN_TABLE
+++++++++++++++++++++

This error is generated when the table referenced does not exist or is unknown.

+++++++++++++++++++++++
YOTTADB
+++++++++++++++++++++++

Octo encountered an error generated by YottaDB. Consult the `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/index.html>`_ or the `Messages and Recovery Procedures Manual <https://docs.yottadb.com/MessageRecovery/index.html>`_ for more information.

-------------------------
Rocto Errors
-------------------------

Errors in Rocto are of the form :code:`ERR_ROCTO_<error>`. The errors are detailed below, in alphabetical order.

.. contents::
   :local:

++++++++++++++++++++++++
INVALID_CHAR_VALUE
++++++++++++++++++++++++

The character value is invalid.

+++++++++++++++++++++++++
INVALID_INT_VALUE
+++++++++++++++++++++++++

The integer value is invalid.

+++++++++++++++++++++++++
INVALID_NUMBER
+++++++++++++++++++++++++

The number of parameters are invalid.

++++++++++++++++++++++
INVALID_TYPE
++++++++++++++++++++++

The variable type used is invalid.

+++++++++++++++++++++
INVALID_VERSION
+++++++++++++++++++++

The version is invalid.

++++++++++++++++++++
MISSING_DATA
++++++++++++++++++++

Data is missing.

++++++++++++++++++
MISSING_NULL
++++++++++++++++++

The null terminator is missing.

++++++++++++++++++++
TOO_FEW_VALUES
++++++++++++++++++++

There are two few values in the message.

+++++++++++++++++
TOO_MANY_VALUES
+++++++++++++++++

There are too many values in the message.

+++++++++++++++++++++++++
TRAILING_CHARS
+++++++++++++++++++++++++

The message has trailing characters.

----------------------------
PostgreSQL Error Codes Used
----------------------------

Octo uses a few `PostgreSQL Error Codes <https://www.postgresql.org/docs/current/errcodes-appendix.html>`_. 

List of PostgreSQL error codes defined and used in Octo:

+----------------------------+-------------------------------------+
| Error Code                 | Condition Name                      |
+============================+=====================================+
| **Class 00 — Successful Completion**                             |
+----------------------------+-------------------------------------+
| 00000                      | successful_completion               |
+----------------------------+-------------------------------------+
| **Class 08 — Connection Exception**                              |
+----------------------------+-------------------------------------+
| 08P01                      | protocol_violation                  |
+----------------------------+-------------------------------------+
| **Class 26 — Invalid SQL Statement Name**                        |
+----------------------------+-------------------------------------+
| 26000                      | invalid_sql_statement_name          |
+----------------------------+-------------------------------------+
| **Class 42 — Syntax Error or Access Rule Violation**             |
+----------------------------+-------------------------------------+
| 42601                      | syntax_error                        |
+----------------------------+-------------------------------------+
