
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

++++++++++++++++++++++++++++
AMBIGUOUS_COLUMN_NAME
+++++++++++++++++++++++++++++

This error is generated when two or more columns from different tables of the same name are given in a single query without qualifying them with their respective table names. PSQL Error Code: 42702

+++++++++++++++++++++
BAD_ADDRESS
+++++++++++++++++++++

This error is issued when Rocto fails to correctly initialize a listening socket. PSQL Error Code: 08000

+++++++++++++++++++++
BAD_CONFIG
+++++++++++++++++++++

This error occurs when invalid configuration settings are used or a syntax error is detected in the configuration file. PSQL Error Code: F0000

+++++++++++++++++++++
BAD_ZROUTINES
+++++++++++++++++++++

This error indicates that no valid source directory was found in octo_zroutines config or in $zroutines ISV. PSQL Error Code: F0000

+++++++++++++++++++++
BUFFER_TOO_SMALL
+++++++++++++++++++++

This error indicates that the buffer used to store output plans is too small for the query. PSQL Error Code: 22000

++++++++++++++++++++++
CALLING_M_ROUTINE
++++++++++++++++++++++

This message is generated when M routines are called. PSQL Error Code: 00000

++++++++++++++++++++++
CLIENT_CONNECTED
++++++++++++++++++++++

This error lets the user know that a client connected successfully. PSQL Error Code: 00000

+++++++++++++++++++++
CURPLAN
+++++++++++++++++++++

This message is a text representation of the current logical plan. PSQL Error Code: 00000

++++++++++++++++++++++
DATABASE_FILES_OOS
++++++++++++++++++++++

This error indicates that the generated routines and the database are not synchronized. PSQL Error Code: XX000

+++++++++++++++++++++++++
ENTERING_FUNCTION
+++++++++++++++++++++++++

This message is generated when a function is entered. PSQL Error Code: 00000

++++++++++++++++++++++++++
FAILED_TO_OPTIMIZE_PLAN
++++++++++++++++++++++++++

This error is generated when Octo failed to optimize a logical plan for a given SQL query. PSQL Error Code: XX000

++++++++++++++++++++++
FAILED_TO_PARSE_SCHEMA
++++++++++++++++++++++

This error is generated when a table failed to parse a schema. PSQL Error Code: XX000

++++++++++++++++++++++++++++
FEATURE_NOT_IMPLEMENTED
++++++++++++++++++++++++++++

This error indicates an attempt to use a feature that is yet to be implemented. PSQL Error Code: 0A000

+++++++++++++++++++++
FILE_NOT_FOUND
+++++++++++++++++++++

This error is generated when Octo tries to read from a file that is non-existent. PSQL Error Code: 58030

++++++++++++++++++++++++++++
GENERATING_TEMPORARY_TABLE
++++++++++++++++++++++++++++

This error is generated during temporary table generation. PSQL Error Code: XX000

+++++++++++++++++
INIT_SCANNER
+++++++++++++++++

This messages indicates an error in intializing the scanner used to parse provided input. Please contact your YottaDB support channel. PSQL Error Code: XX000

+++++++++++++++++++++
INVALID_INPUT_SYNTAX
+++++++++++++++++++++

This error is issued when a user attempts to use a unary '+' or '-' on a field of non-numeric type. PSQL Error Code: 22P02

+++++++++++++++++++++
INVALID_READ_SIZE
+++++++++++++++++++++

This message indicates an internal code attempt to read beyond a buffer's allocated range. Please contact your YottaDB support channel. PSQL Error Code: 22003

+++++++++++++++++++++++
INVALID_WRITE_SIZE
+++++++++++++++++++++++

This message indicates an internal code attempt to write beyond a buffer's allocated range. Please contact your YottaDB support channel. PSQL Error Code: 22003

+++++++++++++++++++++++
JOIN_ALIAS_DUPLICATE
+++++++++++++++++++++++

This error is generated when a table name has been specified more than once. PSQL Error Code: 42712

++++++++++++++++++
LEAVING_FUNCTION
++++++++++++++++++

This message is generated when the flow of control is leaving a function and is used for debugging. PSQL Error Code: 00000

++++++++++++++++++++
LIBSSL_ERROR
++++++++++++++++++++

This error is generated when there is a problem with libssl/libcrypt. PSQL Error Code: XX000

++++++++++++++++++
LINE_TOO_LONG
++++++++++++++++++

This error is generated if the input line is too long. PSQL Error Code: 22026

++++++++++++++++++
LOADING_CONFIG
++++++++++++++++++

This message is generated when a configuration file is being loaded. PSQL Error Code: 00000

++++++++++++++++++++
MISSING_KEY
++++++++++++++++++++

This error indicates that the schema for the table lacks the correct number of keys, and that it needs to be corrected.  PSQL Error Code: 42704

++++++++++++++++++++++++++++
MULTIPLE_ZERO_KEYS
++++++++++++++++++++++++++++

This message indicates that the table has multiple KEY NUM elements with the same number, and that the source schema needs to be corrected. PSQL Error Code: 42P08

+++++++++++++++++++++++++++++
NULL_SUBS_DISABLED
+++++++++++++++++++++++++++++

This error indicates that Null Subscripts have been turned off. However, they must be enabled for proper operation. Set '-null_subscripts=always' for all regions containing Octo global variables. PSQL Error Code: F0000

++++++++++++++++++++++++++++
ORDER_BY_POSITION_INVALID
++++++++++++++++++++++++++++

This error is generated when an ORDER BY clause is incorrectly placed within a SQL query. PSQL Error Code: 42P10

++++++++++++++++++++++++++++++++
ORDER_BY_POSITION_NOT_INTEGER
++++++++++++++++++++++++++++++++

This error is generated when the ORDER BY position is a non-integer. PSQL Error Code: 42601

+++++++++++++++++++
PARSING_COMMAND
+++++++++++++++++++

This message indicates that there is an error in parsing the statement or command. PSQL Error Code: XX000

+++++++++++++++++++++
PARSING_CONFIG
+++++++++++++++++++++

This error is generated when there is an error parsing the configuration file. PSQL Error Code: F0000

++++++++++++++++++++++
PLAN_HASH_FAILED
++++++++++++++++++++++

This error is generated when Octo failed to generate the filename hash for the plan. PSQL Error Code: XX000

+++++++++++++++++++++++
PLAN_NOT_GENERATED
+++++++++++++++++++++++

This error is generated when Octo failed to generate the plan for the given SQL query or command. PSQL Code: XX000

++++++++++++++++++++++
PLAN_NOT_WELL_FORMED
++++++++++++++++++++++

This error is generated when the plan produced by the optimizer is incorrect. Please contact your YottaDB support channel. PSQL Error Code: XX000

++++++++++++++++++++++
PRIMARY_KEY_NOT_FOUND
++++++++++++++++++++++

This error is generated when a table was created without specifying a primary key. PSQL Error Code: 42601

++++++++++++++++++++++++
PROCESSING_MESSAGE_TYPE
++++++++++++++++++++++++

This debug message indicates that a PostgreSQL wire protocol message of a particular type is being processed. PSQL Error Code: 00000

++++++++++++++++++++
READ_MESSAGE
++++++++++++++++++++

This debug message indicates that a PostgreSQL wire protocol message of the specified format was read from the wire. PSQL Error Code: 00000

++++++++++++++++++++
READ_TOO_LARGE
++++++++++++++++++++

This error indicates that a PostgreSQL wire protocol message exceeded the maximum size of messages which can be read by Rocto. Please contact your YottaDB support channel. PSQL Error Code: 22000

++++++++++++++++++
SEND_MESSAGE
++++++++++++++++++

This debug message indicates that a PostgreSQL wire protocol message of the specified format was written to the wire. PSQL Error Code: 00000

++++++++++++++++++++++++++
SETOPER_NUMCOLS_MISMATCH
++++++++++++++++++++++++++

This error is generated when the two operands of a SET operation do not have the same number of columns. PSQL Error Code: 42804

++++++++++++++++++++++++++
SETOPER_TYPE_MISMATCH
++++++++++++++++++++++++++

This error is generated when the two operands of a SET operation are of different types. PSQL Error Code: 42601

++++++++++++++++++++++++++
SUBQUERY_ONE_COLUMN
++++++++++++++++++++++++++

This error is generated when a subquery must return only one column. PSQL Error Code: 42601

++++++++++++++++++++++++++++
SUBQUERY_MULTIPLE_ROWS
++++++++++++++++++++++++++++

This error is generated when more than one row is returned by a subquery that is used as an expression. PSQL Error Code: 21000

+++++++++++++++++
SYSCALL
+++++++++++++++++

This error is generated when a system call has failed. PSQL Error Code: 58000

++++++++++++++++++++++++++
TABLE_DEFINITION_TOO_LONG
++++++++++++++++++++++++++

This error is generated when the table definition is too long. PSQL Error Code: 42P16

+++++++++++++++++++
TYPE_MISMATCH
+++++++++++++++++++

This error is generated when there is a type mismatch between parameters. PSQL Error Code: 42804

+++++++++++++++++++++++
UNKNOWN_COLUMN_NAME
+++++++++++++++++++++++

This error is generated when a column referenced does not exist or is unknown. PSQL Error Code: 42883

++++++++++++++++++++++++++
UNKNOWN_KEYWORD_STATE
++++++++++++++++++++++++++

This error indicates an unknown keyword state was reached. Please contact your YottaDB support channel. PSQL Error Code: XX000

+++++++++++++++++++++++++
UNKNOWN_MESSAGE_TYPE
+++++++++++++++++++++++++

This error is generated when an unknown message type was received from a remote client. Please contact your YottaDB support channel. PSQL Error Code: 08P01

+++++++++++++++++++++
UNKNOWN_TABLE
+++++++++++++++++++++

This error is generated when the table referenced does not exist or is unknown. PSQL Error Code: 42P01

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


++++++++++++++
AUTH_SUCCESS
++++++++++++++

This message indicates that the Rocto user has been successfully authenticated. PSQL Error Code: 00000

+++++++++++++++++++++++
BAD_PASSWORD
+++++++++++++++++++++++

This messages indicates that the password entered does not match the stored value. PSQL Code Error: 28P01

+++++++++++++++++++++++
BAD_TIMESTAMP
+++++++++++++++++++++++

This message indicates that a Cancel Request was attempted using a timestamp that doesn't match that of the target PID. Timestamps are checked to ensure that only the client who spawned a Rocto process can cancel queries running in that process. This error is not disclosed to the client to prevent information leakage about active Rocto processes. PSQL Error Code: 28000

+++++++++++++++++++++++++++++++++++
BIND_PARAMETER_DECODE_FAILURE
+++++++++++++++++++++++++++++++++++

This error indicates that Rocto failed to decode a bind parameter from a binary format. PSQL Error Code: XX000

+++++++++++++++++++++++++
BIND_TO_UNKNOWN_QUERY
+++++++++++++++++++++++++

This error indicates that the user has attempted to bind parameter values to a non-existent prepared statement. PSQL Error Code: 08P01

++++++++++++++++++++++++
CHILD_STATE_UPDATED
++++++++++++++++++++++++

This message indicates that the Rocto child process state has been updated. PSQL Error Code: 00000

+++++++++++++++++++++++
COLUMN_VALUE
+++++++++++++++++++++++

This error indicates that Rocto failed to retrieve the column value from the row. PSQL Error Code: XX000

+++++++++++++++++++++++
CLEAN_DISCONNECT
+++++++++++++++++++++++

This message indicates that Rocto connection has been closed cleanly. PSQL Error Code: 00000

+++++++++++++++++++++++
DB_LOOKUP
+++++++++++++++++++++++

This error is generated when Rocto has failed to retrieve the data from the database. PSQL Error Code: XX000

+++++++++++++++++++++++
HASH_CONVERSION
+++++++++++++++++++++++

This error is generated when Rocto has failed to perform hash conversion. PSQL Error Code: XX000

++++++++++++++++++++++++
INVALID_CHAR_VALUE
++++++++++++++++++++++++

This message indicates that Rocto received an invalid character value in a PostgreSQL wire protocol message. PSQL Error Code: 22000

+++++++++++++++++++++++++
INVALID_INT_VALUE_MULTI
+++++++++++++++++++++++++

This message indicates multiple invalid integer values were provided via a PostgreSQL wire protocol message. PSQL Error Code: 22003

+++++++++++++++++++++++++
INVALID_NUMBER
+++++++++++++++++++++++++

This error indicates that there is an invalid number in the parameter. PSQL Error Code: 22003

++++++++++++++++++++++
INVALID_TYPE
++++++++++++++++++++++

This error indicates that an invalid variable type is used. PSQL Error Code: 08P01

+++++++++++++++++++++
INVALID_VERSION
+++++++++++++++++++++

This error indicates an invalid version has been given as input. PSQL Error Code: 08P01

+++++++++++++++++
M_CALL
+++++++++++++++++

This error is issued when a user attempts to call an M extrinsic function via Rocto. PSQL Error Code: XX000

++++++++++++++++++++
MISSING_DATA
++++++++++++++++++++

This error indicates that there is missing data. PSQL Error Code: 22000

++++++++++++++++++
MISSING_NULL
++++++++++++++++++

This message indicates that a value within a wire protocol message sent by a remote client is missing a null terminator. PSQL Error Code: 22024

++++++++++++++++++++++++++
NONEXISTENT_KEY
++++++++++++++++++++++++++

This messages is generated when there is an invalid authorization specification or a non-existent secret key. PSQL Error Code: 28000

+++++++++++++++++++
NO_SCHEMA
+++++++++++++++++++

This message indicates that Rocto is not allowed to make schema changes without the startup flag set to --allowschemachanges. PSQL Error Code: XX000
 
.. note:: Rocto is yet to implement certain features with respect to Data Manipulation Language (DML) and queries such as INSERT INTO, UPDATE, and DELETE could cause this error to be generated.

+++++++++++++++++++++++++
PARAMETER_STATUS_SENT
++++++++++++++++++++++++

This message indicates that Rocto recorded the value of a database parameter set by a ParameterStatus as part of the PostgreSQL wire protocol startup procedure. PSQL Error Code: 00000

+++++++++++++++++++++++++
PASSWORD_TYPE
+++++++++++++++++++++++++

This message indicates that Rocto received a password encrypted in an unexpected format. PSQL Error Code: 28000

++++++++++++++++++++++++
QUERY_CANCELED
++++++++++++++++++++++++

This message indicates a query was successfully cancelled via a CancelRequest message. PSQL Error Code: 57014

++++++++++++++++++++++++++++
QUERY_TOO_LONG
++++++++++++++++++++++++++++

This message indicates that the query length exceeded maximum size set by STRING_BUFFER_LENGTH in the CMake parameters during configuration. PSQL Error Code: 08P01

++++++++++++++++++++++++
READ_FAILED
++++++++++++++++++++++++

This error is generated when Rocto failed to read data from a remote connection. PSQL Error Code: 08000

++++++++++++++++++++
ROCTO_STARTED
++++++++++++++++++++    

This message indicates a successful start of Rocto on the given port. PSQL Error Code: 00000

+++++++++++++++++++++++
ROCTO_SERVER_FORKED
+++++++++++++++++++++++

This message is generated to show the Rocto server fork that is running, along with it's PID. PSQL Error Code: 00000

++++++++++++++++++++++++++++++
SECRET_KEY_MISMATCH
++++++++++++++++++++++++++++++

This message indicates that the secret key/PID pair doesn't match that expected for a client sending a CancelRequest. PSQL Error Code: 28000

+++++++++++++++++++++++
SEND_FAILED
+++++++++++++++++++++++

This message indicates a failure to send data over the network. PSQL Error Code: 08000

+++++++++++++++++++++++
SEND_MESSAGE
+++++++++++++++++++++++

This message indicates that Rocto failed to send a message of a specific type to a remote client. PSQL Error Code: 08000

+++++++++++++++++++++
SESSION_LOOKUP
+++++++++++++++++++++

This message indicates that Rocto has failed to retrieve the relevant session data for a given client. PSQL Code: XX000

++++++++++++++++++++
TOO_FEW_VALUES
++++++++++++++++++++

This message indicates that a PostgreSQL wire protocol message is missing one or more fields. PSQL Error Code: 22003

+++++++++++++++++
TOO_MANY_VALUES
+++++++++++++++++

This message indicates that a PostgreSQL wire protocol message was submitted with too many fields. PSQL Error Code: 22003

+++++++++++++++++++++++++
TRAILING_CHARS
+++++++++++++++++++++++++

The message indicates that a PostgreSQL wire protocol message has trailing characters. PSQL Error Code: 08P01

++++++++++++++++++++++++++++
UNSUPPORTED_BIND_PARAMETER
++++++++++++++++++++++++++++

This message indicates that Rocto has received a request to bind a value of an unsupported data type to a prepared statement. PSQL Error Code: XX000

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
