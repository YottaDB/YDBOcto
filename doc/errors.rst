
==========================
Error Messages
==========================

.. contents::
   :depth: 4

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

TRACE and DEBUG are used to get helpful output for debugging. INFO provides potentially helpful, but non-critical information about internal operation. WARNING is similar to INFO, but highlights potentially dangerous or undesirable, though non-critical, behavior. ERROR messages report disruptive but recoverable states. Note that ERRORs encountered while parsing or executing a query will cause it to fail. FATAL messages indicate disruptive, unrecoverable states and cause the program to immediately exit, closing any open network connection.

---------------------
Octo or Rocto Errors
---------------------

Octo or Rocto Errors are of the form :code:`ERR_<error>` or :code:`INFO_<error>`. These errors can occur in either :code:`octo` or :code:`rocto`. The errors are detailed below, in alphabetical order. Occurrences of "xxx" indicate portions of the error message text that vary depending on the details of the particular error.

.. contents::
   :local:

++++++++++++++++++++++++++++
ERR_AGGREGATE_FUNCTION_JOIN
++++++++++++++++++++++++++++

Text: Aggregate functions are not allowed in JOIN conditions

Description/Action: This error is generated when aggregate functions are used in JOIN conditions, which is not allowed. PSQL Error Code: 42803

++++++++++++++++++++++++++++++
ERR_AGGREGATE_FUNCTION_NESTED
++++++++++++++++++++++++++++++

Text: Aggregate function calls cannot be nested

Description/Action: This error is generated when aggregate function calls are nested, which is not allowed. PSQL Error Code: 42803

++++++++++++++++++++++++++++
ERR_AGGREGATE_FUNCTION_WHERE
++++++++++++++++++++++++++++

Text: Aggregate functions are not allowed in WHERE

Description/Action: This error is generated when aggregate functions are used in WHERE, which is not allowed. PSQL Error Code: 42803

++++++++++++++++++++++++++++
ERR_AMBIGUOUS_COLUMN_NAME
++++++++++++++++++++++++++++

Text: Ambiguous column name 'xxx': qualify name for safe execution

Description/Action: This error is generated when two or more columns from different tables of the same name are given in a single query without qualifying them with their respective table names. PSQL Error Code: 42702

++++++++++++++++++++++++++++++++
ERR_AS_MORE_COLUMNS
++++++++++++++++++++++++++++++++

Text: Table name 'xxx' has xxx columns available but xxx columns specified

Description/Action: This error is generated when the :code:`AS` keyword specifies a list of column names that is more than the available number of columns. PSQL Error Code: 42P10

+++++++++++++++++++++
ERR_AUTO_UPGRADE
+++++++++++++++++++++

Text: Cannot auto upgrade binary table/function definitions. Please manually upgrade.

Description/Action: Whenever a newer build of Octo is installed in an environment, any internal Octo artifacts that are incompatible with the
newer build are automatically regenerated. This includes the binary representation of table definitions (CREATE TABLE queries)
and function definitions (CREATE FUNCTION queries), If the previous Octo build in use is prior to r1.0.0, this automatic
regeneration is not possible. The ERR_AUTO_UPGRADE error is issued in that case. The manual workaround is to drop/recreate all
tables and functions (using CREATE TABLE, CREATE FUNCTION, DROP TABLE and/or DROP FUNCTION commands) in the environment. PSQL Error Code: F0000

+++++++++++++++++++++
ERR_BAD_CONFIG
+++++++++++++++++++++

Text: Bad config setting in xxx: xxx

Description/Action: This error occurs when invalid configuration settings are used or a syntax error is detected in the configuration file. PSQL Error Code: F0000

+++++++++++++++++++++
ERR_BAD_ZROUTINES
+++++++++++++++++++++

Text: Error no valid source directory found in octo_zroutines config or $zroutines ISV

Description/Action: This error indicates that no valid source directory was found in octo_zroutines config or in $zroutines ISV. PSQL Error Code: F0000

+++++++++++++++++++++
ERR_BUFFER_TOO_SMALL
+++++++++++++++++++++

Text: xxx buffer too small

Description/Action: This error indicates that the buffer used to store output plans is too small for the query. PSQL Error Code: 22000

+++++++++++++++++++++++
INFO_CALLING_M_ROUTINE
+++++++++++++++++++++++

Text: Calling M routine: xxx

Description/Action: This message is generated when M routines are called. PSQL Error Code: 00000

++++++++++++++++++++++++++++
ERR_CANNOT_CREATE_FUNCTION
++++++++++++++++++++++++++++

Text: Function 'xxx' already exists with same argument types

Description/Action: This error indicates an attempt to create an already existing function. PSQL Error Code: 42723

++++++++++++++++++++++++++++
ERR_CANNOT_CREATE_TABLE
++++++++++++++++++++++++++++

Text: Table 'xxx' already exists

Description/Action: This error indicates an attempt to create an already existing table. PSQL Error Code: 42P07

+++++++++++++++++++++++++
ERR_CANNOT_DROP_FUNCTION
+++++++++++++++++++++++++

Text: Cannot DROP function 'xxx': no function defined with given name and parameter types

Description/Action: This error indicates an attempt to drop a non-existent function. PSQL Error Code: 42883

++++++++++++++++++++++++++++++
ERR_CASE_BRANCH_TYPE_MISMATCH
++++++++++++++++++++++++++++++

Text: CASE branch type mismatch: left xxx, right xxx

Description/Action: This error is generated when there is a type mismatch between branches in a :code:`CASE` statement. PSQL Error Code: 42804

++++++++++++++++++++++++++++++
ERR_CASE_VALUE_TYPE_MISMATCH
++++++++++++++++++++++++++++++

Text: WHEN argument is of type xxx but is used in a context that expects a xxx type

Description/Action: This error is generated when there is a type mismatch between :code:`CASE value` and :code:`WHEN condition` type in :code:`Case` statement. PSQL Error Code: 42804

++++++++++++++++++++++
INFO_CLIENT_CONNECTED
++++++++++++++++++++++

Text: Client connected

Description/Action: This message lets the user know that a client connected successfully. PSQL Error Code: 00000

++++++++++++++++++
ERR_COMMON_COLUMN
++++++++++++++++++

Text: Common column name 'xxx' appears more than once in xxx side of NATURAL JOIN

Description/Action: This message is emitted when a user attempts a :code:`NATURAL JOIN` using a table containing multiple columns of the same name, introducing an ambiguity into the join. PSQL Error Code: 42702

++++++++++++++++++++++
ERR_CONFIG_IO_FAILURE
++++++++++++++++++++++

Text: File I/O error reading config setting 'xxx' in config: xxx

Description/Action: This error indicates that a config file setting is unreadable. This typically occurs when a configuration file is not formatted correctly. PSQL Error Code: 58030

+++++++++++++++++++++
INFO_CURPLAN
+++++++++++++++++++++

Text: LOGICAL_PLAN xxx follows xxx

Description/Action: This message is a textual representation of the current logical plan. PSQL Error Code: 00000

+++++++++++++++++++++
INFO_CURSOR
+++++++++++++++++++++

Text: Generating SQL for cursor xxx

Description/Action: This message indicates that SQL instructions are being generated for the given cursor. PSQL Error Code: 00000

++++++++++++++++++++++
ERR_DATABASE_FILES_OOS
++++++++++++++++++++++

Text: Generated routines and database seem to be out of sync

Description/Action: This error indicates that the generated routines and the database are not synchronized. PSQL Error Code: XX000

++++++++++++++++++++++
ERR_DDL_LITERAL
++++++++++++++++++++++

Text: Invalid literal in DDL specification: only xxx literals accepted for this keyword.

Description/Action: This error is emitted when a literal of an invalid type is used in a DDL keyword specification. PSQL Error Code: 42601

++++++++++++++++++++++++
ERR_DOLLAR_SYNTAX
++++++++++++++++++++++++

Text: Prepared statement dollar syntax (e.g. $1, $2, etc.) only supported in Rocto using the Extended Query Protocol

Description/Action: This error indicates that user has tried to use a dollar symbol ($) in Octo. Prepared statement dollar syntax is only supported in Rocto using the PostgreSQL Extended Query Protocol. PSQL Error Code: 42601

++++++++++++++++++++++
ERR_DUPLICATE_COLUMN
++++++++++++++++++++++

Text: Column 'xxx' specified more than once

Description/Action: This error is generated when the :code:`CREATE TABLE` or :code:`INSERT INTO` command specifies more than one column with the same column name. PSQL Error Code: 42701

+++++++++++++++++++++++++
INFO_ENTERING_FUNCTION
+++++++++++++++++++++++++

Text: Entering xxx

Description/Action: This message is generated when a function is entered. PSQL Error Code: 00000

+++++++++++++++++++++++++
INFO_ENV_VAR
+++++++++++++++++++++++++

Text: # xxx='xxx'

Description/Action: This message reports the current value of a YottaDB environment variable. PSQL Error Code: 00000

++++++++++++++++++++++++++++
ERR_FAILED_TO_OPTIMIZE_PLAN
++++++++++++++++++++++++++++

Text: Failed to optimize logical plan

Description/Action: This error is generated when Octo fails to optimize a logical plan for a given SQL query. PSQL Error Code: XX000

+++++++++++++++++++++++++++
ERR_FAILED_TO_PARSE_SCHEMA
+++++++++++++++++++++++++++

Text: Failed to parse schema from table xxx

Description/Action: This error is generated when a table fails to parse a schema. PSQL Error Code: XX000

+++++++++++++++++++++++++++++++++++++++++++++
ERR_FAILED_TO_RETRIEVE_ENVIRONMENT_VARIABLE
+++++++++++++++++++++++++++++++++++++++++++++

Text: Failed to retrieve value of environment variable: 'xxx'

Description/Action: This error is generated when Octo fails to retrieve the value of an environment variable. PSQL Error Code: F0000

++++++++++++++++++++++++++++
ERR_FEATURE_NOT_IMPLEMENTED
++++++++++++++++++++++++++++

Text: Feature not implemented: xxx

Description/Action: This error indicates an attempt to use a feature that is yet to be implemented. PSQL Error Code: 0A000

+++++++++++++++++++++
ERR_FILE_NOT_FOUND
+++++++++++++++++++++

Text: Error opening input file: xxx

Description/Action: This error is generated when Octo tries to read from a file that is non-existent. PSQL Error Code: 58030

++++++++++++++++++++++
INFO_FUNCTION_SEARCH
++++++++++++++++++++++

Text: Searching for function xxx

Description/Action: This message lets the user know that Octo is looking up the given function. PSQL Error Code: 00000

+++++++++++++++++++++++++++++++
ERR_GENERATING_TEMPORARY_TABLE
+++++++++++++++++++++++++++++++

Text: Generating temporary table: xxx

Description/Action: This error is generated during temporary table generation. PSQL Error Code: XX000

++++++++++++++++++++
INFO_GENERATING_XREF
++++++++++++++++++++

Text: Generating helper cross reference M file [xxx] for table [xxx] and column [xxx]

Description/Action: This message indicates that a cross reference is being generated for the given table and column as an optimization. PSQL Error Code: XX000

++++++++++++++++++++++++++++++
ERR_GROUP_BY_ONLY_COLUMN_NAME
++++++++++++++++++++++++++++++

Text: Only column names are valid in GROUP BY (not expressions or constants)

Description/Action: This error is generated when expressions or constants are used in :code:`GROUP BY`. Please use only valid column names. PSQL Error Code: 42803

++++++++++++++++++++++++++++++++++++
ERR_GROUP_BY_OR_AGGREGATE_FUNCTION
++++++++++++++++++++++++++++++++++++

Text: Column xxx must appear in the GROUP BY clause or be used in an aggregate function

Description/Action: This error is generated when a column is :code:`SELECT` ed, but does not appear in a :code:`GROUP BY` clause or isn't used in an aggregate function. PSQL Error Code: 42803

+++++++++++++++++
ERR_IDENT_LENGTH
+++++++++++++++++

Text: xxx length xxx exceeds max (xxx)

Description/Action: This error indicates that a query attempted to use an identifier (such as a table name) that is longer than the maximum allowed. PSQL Error Code: 22P02

+++++++++++++++++
ERR_INIT_SCANNER
+++++++++++++++++

Text: Error initializing the scanner

Description/Action: This message indicates an error in initializing the scanner used to parse provided input. Please contact your YottaDB support channel. PSQL Error Code: XX000

++++++++++++++++++++++++++++
ERR_INSERT_TOO_MANY_COLUMNS
++++++++++++++++++++++++++++

Text: INSERT has more target columns than expressions

Description/Action: This error is generated when the :code:`INSERT INTO` command specifies more columns in the comma-separated list of columns (corresponding to the target table) than the number of expression columns in the source query. PSQL Error Code: 42601

++++++++++++++++++++++++++++++++
ERR_INSERT_TOO_MANY_EXPRESSIONS
++++++++++++++++++++++++++++++++

Text: INSERT has more expressions than target columns

Description/Action: This error is generated when the :code:`INSERT INTO` command specifies more expression columns in the source query than the number of comma-separated columns specified for the target table (if a comma-separated list of target columns is specified) or the number of columns of the target table (if no comma-separated list of target columns is specified). PSQL Error Code: 42601

++++++++++++++++++++++++++++++
ERR_INSERT_TYPE_MISMATCH
++++++++++++++++++++++++++++++

Text: Column 'xxx' is of type xxx but expression is of type xxx

Description/Action: This error is generated when the :code:`INSERT INTO` command specifies a target column whose type is different from the corresponding source column expression. PSQL Error Code: 42804

+++++++++++++++++++++
ERR_INVALID_AS_SYNTAX
+++++++++++++++++++++

Text: Invalid AS value type: xxx

Description/Action: This error is issued when a user attempts to use an unacceptable type for an :code:`AS` value. PSQL Error Code: 22P02

++++++++++++++++++++++
ERR_INVALID_CLI_OPTION
++++++++++++++++++++++

Text: Invalid value specified for option xxx

Description/Action: This error is issued when a user attempts to use an unacceptable command line option value. PSQL Error Code: 22P02

+++++++++++++++++++++++++++
ERR_INVALID_ESCAPE_PATTERN
+++++++++++++++++++++++++++

Text: Cannot end pattern with escape character: xxx

Description/Action: This error is issued when a user attempts to use an invalid escape pattern in a regular expression. PSQL Error Code: 22025

+++++++++++++++++++++++++
ERR_INVALID_INPUT_SYNTAX
+++++++++++++++++++++++++

Text: Invalid input syntax : Expecting type NUMERIC or INTEGER : Actual type xxx

Description/Action: This error is issued when a user attempts to use a unary '+' or '-' on a field of non-numeric type. PSQL Error Code: 22P02

++++++++++++++++++++++++++++++
ERR_INVALID_INPUT_SYNTAX_BOOL
++++++++++++++++++++++++++++++

Text: Invalid input syntax for type boolean: 'xxx' is not a valid boolean value

Description/Action: This error is generated when the user attempts to assign a non-boolean value to a boolean variable. PSQL Error Code: 22P02

+++++++++++++++++++++++++++++
ERR_INVALID_INTEGER_SYNTAX
+++++++++++++++++++++++++++++

Text: Invalid input syntax for type integer: 'xxx'

Description/Action: This error indicates that an integer was expected in this context, but a non-integer value was specified. PSQL Error Code: 22P02

+++++++++++++++++++++++++
ERR_INVALID_KEYWORD_CHAR
+++++++++++++++++++++++++

Text: Integer value xxx given for xxx character is not a valid ASCII (0-127) character

Description/Action: This error is issued when a user attempts to use an invalid character as part of a :code:`DELIM` or :code:`NULLCHAR` specfication. Accepted values range from 0 to 127 (ASCII). PSQL Error Code: 42601

+++++++++++++++++++++++++
ERR_INVALID_NUMBER
+++++++++++++++++++++++++

Text: xxx: invalid number of xxx: xxx is out of range (min: xxx, max: xxx)

Description/Action: This error indicates that there is an invalid number of items of the given type. PSQL Error Code: 22003

+++++++++++++++++++++++++++
ERR_INVALID_READ_SIZE
+++++++++++++++++++++++++++

Text: Read size xxx out of range

Description/Action: This error indicates an internal code attempt to read beyond a buffer's allocated range. Please contact your YottaDB support channel. PSQL Error Code: 22003

+++++++++++++++++++++++
ERR_INVALID_TYPE
+++++++++++++++++++++++

Text: Type xxx does not exist

Description/Action: This error is generated when a user attempts to use a type that doesn't exist. PSQL Error Code: 42704

+++++++++++++++++++++++
ERR_INVALID_WRITE_SIZE
+++++++++++++++++++++++

Text: Write size xxx out of range

Description/Action: This error indicates an internal code attempt to write beyond a buffer's allocated range. Please contact your YottaDB support channel. PSQL Error Code: 22003

+++++++++++++++++++++++++
ERR_JOIN_ALIAS_DUPLICATE
+++++++++++++++++++++++++

Text: table name 'xxx' specified more than once

Description/Action: This error is generated when a table name has been specified more than once. PSQL Error Code: 42712

++++++++++++++++++++++
INFO_LEAVING_FUNCTION
++++++++++++++++++++++

Text: LEAVING xxx

Description/Action: This message is generated when the flow of control is leaving a function and is used for debugging. PSQL Error Code: 00000

++++++++++++++++++++
ERR_LIBCALL
++++++++++++++++++++

Text: Library call failed: xxx

Description/Action: This error is generated when a library call fails. PSQL Error Code: 58000

++++++++++++++++++++
ERR_LIBCALL_WITH_ARG
++++++++++++++++++++

Text: Library call xxx failed with argument 'xxx'

Description/Action: This error is generated when a library call fails, and provides additional information about the arguments passed to it. PSQL Error Code: 58000

++++++++++++++++++++
ERR_LIBSSL_ERROR
++++++++++++++++++++

Text: Error in libssl/libcrypt

Description/Action: This error is generated when there is a problem with libssl/libcrypt. PSQL Error Code: XX000

++++++++++++++++++
ERR_LINE_TOO_LONG
++++++++++++++++++

Text: Input line too long

Description/Action: This error is generated if the input line is too long. PSQL Error Code: 22026

+++++++++++++++++++
ERR_LITERAL_MAX_LEN
+++++++++++++++++++

Text: Literal value exceeds max length (xxx bytes)

Description/Action: This error indicates that a query was issued containing a literal value that exceeds the maximum length in bytes allowed for any literal value. PSQL Error Code: 22003

++++++++++++++++++
INFO_LOADED_CONFIG
++++++++++++++++++

Text: Loaded config from xxx

Description/Action: This message is generated when a configuration file has been loaded. PSQL Error Code: 00000

++++++++++++++++++
INFO_M_PLAN
++++++++++++++++++

Text: Generating M file [xxx] (to execute SQL query)

Description/Action: This message notifies the user that an M plan is being generated for the given cursor. PSQL Error Code: 00000

++++++++++++++++++++++
INFO_MEM_REALLOCATION
++++++++++++++++++++++

Text: Memory xxx for variable xxx

Description/Action: This message is generated when memory for a particular variable is reallocated. PSQL Error Code: 00000

++++++++++++++++++++
ERR_MEMORY_USAGE
++++++++++++++++++++

Text: Failed to retrieve memory usage at process exit

Description/Action: This message indicates a failure to determine how much memory is in use at Octo process exit. PSQL Error Code: 58000

++++++++++++++++++++
INFO_MEMORY_USAGE
++++++++++++++++++++

Text: Memory usage at process exit: xxx Kb

Description/Action: This message reports how much memory is in use at Octo process exit. PSQL Error Code: 00000

++++++++++++++++++++
ERR_MIDENT_LENGTH
++++++++++++++++++++

Text: Length xxx too large for M identifier (max length xxx)

Description/Action: This error indicates that the length for an M identifier has been exceeded. PSQL Error Code: 22003

+++++++++++++++++++++++
ERR_MISSING_FROM_ENTRY
+++++++++++++++++++++++

Text: Missing FROM-clause entry for table : xxx

Description/Action: This error indicates that there is no entry for the given table in a FROM clause. PSQL Error Code: 42P01

++++++++++++++++++++
ERR_MISSING_KEY
++++++++++++++++++++

Text: Missing key xxx in table xxx; max key was xxx

Description/Action: This error indicates that the schema for the table lacks the correct number of keys, and that it needs to be corrected.  PSQL Error Code: 42704

++++++++++++++++++++++
ERR_MISTYPED_FUNCTION
++++++++++++++++++++++

Text: Function xxx cannot be invoked with a parameter of type xxx

Description/Action: This error indicates that a function was passed an argument whose type does not match that defined for the given parameter. PSQL Error Code: 42883

++++++++++++++++++++++++++++
ERR_MULTIPLE_VALUES_PROVIDED
++++++++++++++++++++++++++++

Text: Multiple values provided for xxx; undefined behavior

Description/Action: This error indicates that multiple values have been provided for a particular parameter. PSQL Error Code: 42P08

++++++++++++++++++++++++++++
ERR_MULTIPLE_ZERO_KEYS
++++++++++++++++++++++++++++

Text: Multiple xxx keys found for table xxx

Description/Action: This error indicates that the table has multiple :code:`KEY NUM` elements with the same number, and that the source schema needs to be corrected. PSQL Error Code: 42P08

+++++++++++++++++++++++++++++
ERR_NULL_SUBS_DISABLED
+++++++++++++++++++++++++++++

Text: Null subscripts must be enabled for proper operation. Please set '-null_subscripts=always' for all regions containing Octo global variables.

Description/Action: This error indicates that Null Subscripts have been turned off. However, they must be enabled for proper operation. Set '-null_subscripts=always' for all regions containing Octo global variables. PSQL Error Code: F0000

+++++++++++++++++++++++++++++
ERR_NUMERIC_SCALE
+++++++++++++++++++++++++++++

Text: Numeric scale xxx must be between 0 and precision xxx

Description/Action: This error indicates that the SCALE value in a NUMERIC(PRECISION,SCALE) specification cannot be greater than the PRECISION value. PSQL Error Code: 22023

+++++++++++++++++++++++++++++
ERR_NUMERIC_OVERFLOW
+++++++++++++++++++++++++++++

Text: Numeric field overflow; A field with precision xxx, scale xxx must round to an absolute value less than 10^xxx

Description/Action: This error indicates that the integer portion of a value that is being type cast to NUMERIC(PRECISION,SCALE) cannot be longer than PRECISION-SCALE (i.e. PRECISION minus SCALE) decimal digits. PSQL Error Code: 22003

++++++++++++++++++++++++++++++
ERR_ORDER_BY_POSITION_INVALID
++++++++++++++++++++++++++++++

Text: ORDER BY position xxxxxx is not in select list

Description/Action: This error is generated when an ORDER BY clause is incorrectly placed within a SQL query. PSQL Error Code: 42P10

++++++++++++++++++++++++++++++++++
ERR_ORDER_BY_POSITION_NOT_INTEGER
++++++++++++++++++++++++++++++++++

Text: Non integer constant xxxxxx in ORDER BY

Description/Action: This error is generated when the ORDER BY position is a non-integer. PSQL Error Code: 42601

++++++++++++++++++++++++++++++++
ERR_ORDER_BY_SELECT_DISTINCT
++++++++++++++++++++++++++++++++

Text: For SELECT DISTINCT, ORDER BY expressions must appear in select list

Description/Action: This error is generated when the :code:`ORDER BY` expression is attempted outside of the select list when using the :code:`DISTINCT` quantifier. PSQL Error Code: 42P10

+++++++++++++++++++
INFO_OCTO_STARTED
+++++++++++++++++++

Text: Octo started

Description/Action: This message indicates that an Octo process has begun execution. PSQL Error Code: 00000

+++++++++++++++++++
ERR_PARSING_COMMAND
+++++++++++++++++++

Text: Error parsing statement: xxx

Description/Action: This message indicates that there is an error in parsing the statement or command. PSQL Error Code: XX000

+++++++++++++++++++++
ERR_PARSING_CONFIG
+++++++++++++++++++++

Text: Error parsing config (xxx): line xxx: xxx

Description/Action: This error is generated when there is an error parsing the configuration file. PSQL Error Code: F0000

+++++++++++++++++++
INFO_PARSING_DONE
+++++++++++++++++++

Text: Parsing done for SQL command [xxx]

Description/Action: This message indicates that parsing is complete for the given SQL statement or command. PSQL Error Code: 00000

++++++++++++++++++++++++++++++++++++++++
ERR_PERCENT_IN_EXTRINSIC_FUNCTION_NAME
++++++++++++++++++++++++++++++++++++++++

Text: '%%' is only allowed at the beginning of an M label or routine name.

Description/Action: This error is generated when a user attempts to map a SQL function to an improperly formatted M extrinsic function name. PSQL Error Code: 42601

++++++++++++++++++++++
ERR_PLAN_HASH_FAILED
++++++++++++++++++++++

Text: Failed to generate plan filename hash

Description/Action: This error is generated when Octo fails to generate the filename hash for the plan. PSQL Error Code: XX000

+++++++++++++++++++++++
ERR_PLAN_NOT_GENERATED
+++++++++++++++++++++++

Text: Failed to generate xxx plan

This error is generated when Octo fails to generate the plan for the given SQL query or command. PSQL Code: XX000

+++++++++++++++++++++++++
ERR_PLAN_NOT_WELL_FORMED
+++++++++++++++++++++++++

Text: Plan produced by optimizer appears incorrect

Description/Action: This error is generated when the plan produced by the optimizer is incorrect. Please contact your YottaDB support channel. PSQL Error Code: XX000

++++++++++++++
ERR_PLAN_OWNER
++++++++++++++

Text: Problem resolving owner for deferred plan; undefined behavior

Description/Action: This error is indicates an internal error in resolving query subplans. Please contact your YottaDB support channel. PSQL Error Code: XX000

++++++++++++++++++++++++++
ERR_PRIMARY_KEY_NOT_FOUND
++++++++++++++++++++++++++

Text: No primary key specified when creating table 'xxx'. Please consult the documentation for more information.

Description/Action: This error is generated when a table was created without specifying a primary key. PSQL Error Code: 42601

+++++++++++++++++++++++++++++
INFO_PROCESSING_MESSAGE_TYPE
+++++++++++++++++++++++++++++

Text: Processing message type xxx

Description/Action: This debug message indicates that a PostgreSQL wire protocol message of a particular type is being processed. PSQL Error Code: 00000

++++++++++++++++++++++++
ERR_READWRITE_DISALLOWED
++++++++++++++++++++++++

Text: READWRITE disallowed if table-level GLOBAL or column-level EXTRACT/PIECE/GLOBAL/DELIM/START/STARTINCLUDE/END are specified

Description/Action: This error is generated when a CREATE TABLE command specifies the table type to be READWRITE but also specifies another keyword that is incompatible with READWRITE. A table level GLOBAL or a column level EXTRACT, PIECE, GLOBAL, DELIM, START, STARTINCLUDE or END are keywords that are not compatible with READWRITE. PSQL Error Code: 42601

++++++++++++++++++++
INFO_READ_MESSAGE
++++++++++++++++++++

Text: Read message of type xxx and length xxx

Description/Action: This debug message indicates that a PostgreSQL wire protocol message of the specified format was read from the wire. PSQL Error Code: 00000

+++++++++++++++++++++++
INFO_RECORDING_ENV_VARS
+++++++++++++++++++++++

Text: # Recording pertinent ydb_* env var values at process startup

Description/Action: This message notes that various YDB environment variable values are being recorded at startup . PSQL Error Code: 00000

+++++++++++++++++++++++
INFO_RETURNING_FAILURE
+++++++++++++++++++++++

Text: Returning failure from xxx

Description/Action: This debug message indicates that the given function exited due to an error. PSQL Error Code: 00000

++++++++++++++++++++
INFO_REUSE_M_PLAN
++++++++++++++++++++

Text: Using already generated M file [xxx] (to execute SQL query)

Description/Action: This message indicates that an M plan has already been generated for the current SQL query and will be used instead of creating a new one. PSQL Error Code: 00000

+++++++++++++++++++++
SELECT_STAR_NO_TABLES
+++++++++++++++++++++

Text: SELECT * with no tables specified is not valid

Description/Action: This error indicates that a user attempted to select all rows without specifying a table to select them from. PSQL Error Code: 42601

++++++++++++++++++
INFO_SEND_MESSAGE
++++++++++++++++++

Text: Sending message of type xxx and length xxx

Description/Action: This debug message indicates that a PostgreSQL wire protocol message of the specified format was written to the wire. PSQL Error Code: 00000

+++++++++++++++++++++++++++++
ERR_SETOPER_NUMCOLS_MISMATCH
+++++++++++++++++++++++++++++

Text: Each xxx query must have same number of columns

Description/Action: This error is generated when the two operands of a SET operation do not have the same number of columns. PSQL Error Code: 42804

++++++++++++++++++++++++++
ERR_SETOPER_TYPE_MISMATCH
++++++++++++++++++++++++++

Text: xxx types xxx and xxx cannot be matched

Description/Action: This error is generated when the two operands of a SET operation are of different types. PSQL Error Code: 42601

++++++++++++++++++++++++++
ERR_SUBQUERY_ONE_COLUMN
++++++++++++++++++++++++++

Text: Subquery must return only one column

Description/Action: This error is generated when a subquery must return only one column. PSQL Error Code: 42601

++++++++++++++++++++++++++++
ERR_SUBQUERY_MULTIPLE_ROWS
++++++++++++++++++++++++++++

Text: More than one row returned by a subquery used as an expression

Description/Action: This error is generated when more than one row is returned by a subquery that is used as an expression. PSQL Error Code: 21000

+++++++++++++++++
ERR_SYSCALL
+++++++++++++++++

Text: System call failed: xxx, return xxx (xxx)

Description/Action: This error is generated when a system call has failed. PSQL Error Code: 58000

+++++++++++++++++++++++
ERR_SYSCALL_WITH_ARG
+++++++++++++++++++++++

Text: System call failed: xxx, return xxx (xxx): args: xxx

Description/Action: This error is generated when a system call fails, and provides additional information about the arguments passed to it. PSQL Error Code: 58000

++++++++++++++++++++++++++++++
ERR_TABLE_DEFINITION_TOO_LONG
++++++++++++++++++++++++++++++

Text: Table definition for xxx too long; max size is xxx, table length is xxx

Description/Action: This error is generated when the table definition is too long. PSQL Error Code: 42P16

++++++++++++++++++++++
INFO_TABLE_SEARCH
++++++++++++++++++++++

Text: Searching for table xxx

Description/Action: This message lets the user know that Octo is looking up the given table. PSQL Error Code: 00000

+++++++++++++++++++++++++
INFO_TEXT_REPRESENTATION
+++++++++++++++++++++++++

Text: xxx

Description/Action: This message prints the text representation of a DDL specification. PSQL Error Code: 00000

+++++++++++++++++++
ERR_TABLE_READONLY
+++++++++++++++++++

Text: xxx not allowed on READONLY tables. Only allowed on READWRITE tables.

Description/Action: Queries that modify tables (e.g. INSERT INTO, DELETE, ALTER etc.) are not allowed on tables that have been created as READONLY. They are only allowed on READWRITE tables. PSQL Error Code: 42601

++++++++++++++++++++++++++++++
ERR_TABLE_UNKNOWN_COLUMN_NAME
++++++++++++++++++++++++++++++

Text: Column 'xxx' of table 'xxx' does not exist

Description/Action: This error is generated when the specified column name is not a valid column in the specified table. PSQL Error Code: 42703

++++++++++++++++++++++++++++++++
ERR_TOO_MANY_DELIM_CHARS
++++++++++++++++++++++++++++++++

Text: Too many characters specified for DELIM specification (got: xxx, max: xxx)

Description/Action: This message indicates an attempt to specify more characters in a DELIM specification than is supported within an Octo DDL. PSQL Error Code: 22003

++++++++++++++++++++++++++++++++
ERR_TOO_MANY_FUNCTION_ARGUMENTS
++++++++++++++++++++++++++++++++

Text: Too many arguments passed for function xxx (max: xxx)

Description/Action: This error indicates an attempt to create a function with more arguments than the maximum allowed. PSQL Error Code: 22003

+++++++++++++++++++
ERR_TYPE_MISMATCH
+++++++++++++++++++

Text: Type mismatch: left xxx, right xxx

Description/Action: This error is generated when there is a type mismatch between parameters. PSQL Error Code: 42804

++++++++++++++++++++++++
ERR_TYPE_NOT_COMPATIBLE
++++++++++++++++++++++++

Text: Type xxx not compatible for xxx

Description/Action: This error is generated when a type is not compatible with a parameter. PSQL Error Code: 42883

+++++++++++++++++++++++
ERR_UNKNOWN_COLUMN_NAME
+++++++++++++++++++++++

Text: Unknown column: xxx

Description/Action: This error is generated when the column referenced does not exist or is unknown. PSQL Error Code: 42703

++++++++++++++++++++++++++
ERR_UNKNOWN_FUNCTION
++++++++++++++++++++++++++

Text: No function 'xxx' defined with given parameter types (xxx)

Description/Action: This error is generated when the function referenced does not exist or is unknown. PSQL Error Code: 42883

++++++++++++++++++++++++++
ERR_UNKNOWN_KEYWORD_STATE
++++++++++++++++++++++++++

Text: Unknown state reached; please contact your Octo support channel

Description/Action: This error indicates an unknown keyword state was reached. Please contact your YottaDB support channel. PSQL Error Code: XX000

+++++++++++++++++++++++++
ERR_UNKNOWN_MESSAGE_TYPE
+++++++++++++++++++++++++

Text: Unknown message type from frontend: xxx

Description/Action: This error is generated when an unknown message type was received from a remote client. Please contact your YottaDB support channel. PSQL Error Code: 08P01

+++++++++++++++++++++
ERR_UNKNOWN_TABLE
+++++++++++++++++++++

Text: Unknown table: xxx

Description/Action: This error is generated when the table referenced does not exist or is unknown. PSQL Error Code: 42P01

++++++++++++++++++
ERR_VALUES_LENGTH
++++++++++++++++++

Text: VALUES lists must all be the same length

Description/Action: This error is generated when a VALUES keyword specifies a list of rows where at least one row does not have the same number of columns as the other rows. PSQL Error Code: 42601

+++++++++++++++++++++
ERR_VARCHAR_TOO_LONG
+++++++++++++++++++++

Text: Value too long for type VARCHAR(xxx)

Description/Action: This error indicates that the specified value is more than xxx characters long and hence cannot fit in the VARCHAR(xxx) type. Specify a value which is less than or equal to xxx characters long. PSQL Error Code: 22001

+++++++++++++++++++++++
ERR_YOTTADB
+++++++++++++++++++++++

Text: YottaDB error: xxx

Octo encountered an error generated by YottaDB. Consult the `Administration and Operations Guide <https://docs.yottadb.com/AdminOpsGuide/index.html>`_ or the `Messages and Recovery Procedures Manual <https://docs.yottadb.com/MessageRecovery/index.html>`_ for more information.

-------------------------
Rocto Specific Errors
-------------------------

Rocto Specific Errors are of the form :code:`ERR_ROCTO_<error>` or :code:`INFO_ROCTO_<error>`. These errors can occur only in :code:`rocto`. The errors are detailed below, in alphabetical order. Occurrences of "xxx" indicate portions of the error message text that vary depending on the details of the particular error.

.. contents::
   :local:


++++++++++++++++++
INFO_AUTH_SUCCESS
++++++++++++++++++

Text: xxx: user successfully authenticated

Description/Action: This message indicates that the Rocto user has been successfully authenticated. PSQL Error Code: 00000

+++++++++++++++++++++
ERR_ROCTO_BAD_ADDRESS
+++++++++++++++++++++

Text: Bad listen address: xxx

Description/Action: This error is issued when Rocto fails to correctly initialize a listening socket. PSQL Error Code: 08000

+++++++++++++++++++++++
ERR_ROCTO_BAD_PASSWORD
+++++++++++++++++++++++

Text: xxx: password doesn't match stored value

This message indicates that the password entered does not match the stored value. PSQL Code Error: 28P01

+++++++++++++++++++++++
ERR_ROCTO_BAD_TIMESTAMP
+++++++++++++++++++++++

Text: handle_cancel_request: PID timestamp doesn't match stored value

Description/Action: This message indicates that a Cancel Request was attempted using a timestamp that doesn't match that of the target PID. Timestamps are checked to ensure that only the client who spawned a Rocto process can cancel queries running in that process. This error is not disclosed to the client to prevent information leakage about active Rocto processes. PSQL Error Code: 28000

++++++++++++++++++++++++++++++++++++++++
ERR_ROCTO_BIND_PARAMETER_DECODE_FAILURE
++++++++++++++++++++++++++++++++++++++++

Text: Failed to decode binary bind parameter

Description/Action: This error indicates that Rocto failed to decode a bind parameter from a binary format. PSQL Error Code: XX000

++++++++++++++++++++++++++++++++
ERR_ROCTO_BIND_TO_UNKNOWN_QUERY
++++++++++++++++++++++++++++++++

Text: Bind to unknown query attempted

Description/Action: This error indicates that the user has attempted to bind parameter values to a non-existent prepared statement. PSQL Error Code: 08P01

+++++++++++++++++++++++++++++++
INFO_ROCTO_CHILD_STATE_UPDATED
+++++++++++++++++++++++++++++++

Text: Process xxx switched to state xxx

Description/Action: This message indicates that the Rocto child process state has been updated. PSQL Error Code: 00000

++++++++++++++++++++++++++++
INFO_ROCTO_CLEAN_DISCONNECT
++++++++++++++++++++++++++++

Text: connection closed cleanly

Description/Action: This message indicates that a Rocto connection has been closed cleanly. PSQL Error Code: 00000

+++++++++++++++++++++++
ERR_ROCTO_COLUMN_VALUE
+++++++++++++++++++++++

Text: xxx: failed to extract column value xxx from row

Description/Action: This error indicates that Rocto failed to retrieve the column value from the row. PSQL Error Code: XX000

+++++++++++++++++++++++
ERR_ROCTO_COMMAND_TAG
+++++++++++++++++++++++

Text: Failed to identify command tag

Description/Action: This error indicates that Rocto failed to identify the command tag. PSQL Error Code: XX000

+++++++++++++++++++++++
ERR_ROCTO_DB_LOOKUP
+++++++++++++++++++++++

Text: xxx: failed to retrieve xxx from database

Description/Action: This error is generated when Rocto has failed to retrieve the data from the database. PSQL Error Code: XX000

++++++++++++++++++++++++++
ERR_ROCTO_HASH_CONVERSION
++++++++++++++++++++++++++

Text: xxx: failed convert xxx hash to xxx

Description/Action: This error is generated when Rocto has failed to perform hash conversion. PSQL Error Code: XX000

+++++++++++++++++++++++++++
ERR_ROCTO_INVALID_INT_VALUE
+++++++++++++++++++++++++++

Text: xxx: invalid xxx value xxx: must be xxx

Description/Action: This error indicates that Rocto received an invalid integer value in a PostgreSQL wire protocol message. PSQL Error Code: 22003

++++++++++++++++++++++++++++
ERR_ROCTO_INVALID_ITEM_VALUE
++++++++++++++++++++++++++++

Text: xxx: invalid item value xxx: must be xxx

Description/Action: This error indicates that Rocto received an invalid value for the 'item' field of a message of the specified PostgreSQL wire protocol message type. PSQL Error Code: 22000

++++++++++++++++++++++++++++++
ERR_ROCTO_INVALID_MESSAGE_TYPE
++++++++++++++++++++++++++++++

Text: xxx: invalid type 'xxx': must be 'xxx'

Description/Action: This error indicates that an invalid PostgreSQL wire protocol message type was used. PSQL Error Code: 08P01

++++++++++++++++++++++++++++++++++++++++
ERR_ROCTO_INVALID_NUMBER_BIND_PARAMETERS
++++++++++++++++++++++++++++++++++++++++

Text: xxx: invalid number of parameters: expected xxx got xxx)

Description/Action: This error indicates that an invalid number of parameters have been provided for a Bind message. PSQL Error Code: 22003

++++++++++++++++++++++++++++++++++++++++++++
ERR_ROCTO_INVALID_NUMBER_COLUMN_FORMAT_CODES
++++++++++++++++++++++++++++++++++++++++++++

Text: xxx: invalid number of column format codes specified for portal xxx: expected xxx got xxx)

Description/Action: This error occurs when a client requests a different number of output column formats than the number of columns to be returned. PSQL Error Code: 22P02

++++++++++++++++++++++++++++++++++
ERR_ROCTO_INVALID_FORMAT_CODE
++++++++++++++++++++++++++++++++++

Text: Bind: invalid xxx format code xxx: must be 0 (text) or 1 (binary)

Description/Action: This error indicates multiple invalid integer values were provided via a PostgreSQL wire protocol message. PSQL Error Code: 22003

++++++++++++++++++++++++++
ERR_ROCTO_INVALID_VERSION
++++++++++++++++++++++++++

Text: xxx: invalid version xxx: must be xxx

Description/Action: This error indicates an invalid version has been given as input. PSQL Error Code: 08P01

+++++++++++++++++
ERR_M_CALL
+++++++++++++++++

Text: Inline M function calls are disallowed. Use CREATE FUNCTION instead to map it to an SQL function.

Description/Action: This error is issued when a user attempts to call an M extrinsic function via Rocto. PSQL Error Code: XX000

++++++++++++++++++++++++
ERR_ROCTO_MISSING_DATA
++++++++++++++++++++++++

Text: xxx: missing xxx

Description/Action: This error indicates that there is missing data. PSQL Error Code: 22000

+++++++++++++++++++++++
ERR_ROCTO_MISSING_NULL
+++++++++++++++++++++++

Text: xxx: xxx missing null terminator

Description/Action: This error indicates that a value within a wire protocol message sent by a remote client is missing a null terminator. PSQL Error Code: 22024

+++++++++++++++++++++++++++
ERR_ROCTO_MISSING_USERNAME
+++++++++++++++++++++++++++

Text: xxx: startup message missing username

Description/Action: This error indicates that a client attempted to initiate remote connection without specifying a username. PSQL Error Code: 08P01

++++++++++++++++++++++++++
ERR_ROCTO_NONEXISTENT_KEY
++++++++++++++++++++++++++

Text: handle_cancel_request: received non-existent secret key

Description/Action: This error is generated when there is an invalid authorization specification or a non-existent secret key. PSQL Error Code: 28000

+++++++++++++++++++
ERR_ROCTO_NO_SCHEMA
+++++++++++++++++++

Text: Rocto is not allowed to make schema changes without startup flag --allowschemachanges

Description/Action: This error indicates that Rocto is not allowed to make schema changes without the startup flag set to :code:`--allowschemachanges`. PSQL Error Code: XX000

.. note:: Rocto is yet to implement certain features with respect to Data Manipulation Language (DML) and queries such as INSERT INTO, UPDATE, and DELETE could cause this error to be generated.

++++++++++++++++++++++++++
ERR_ROCTO_PARAMETER_COUNT
++++++++++++++++++++++++++

Text: Failed to count number of parameters in prepared statement

Description/Action: This error indicates that Rocto failed to count the number of parameters provided in the prepared statement. PSQL Error Code: XX000

+++++++++++++++++++++++++
ERR_ROCTO_PASSWORD_TYPE
+++++++++++++++++++++++++

Text: xxx: expected xxx encrypted password

Description/Action: This error indicates that Rocto received a password encrypted in an unexpected format. PSQL Error Code: 28000

++++++++++++++++++++++++
ERR_ROCTO_QUERY_CANCELED
++++++++++++++++++++++++

Text: canceling statement due to user request

Description/Action: This error indicates a query was successfully cancelled via a CancelRequest message. PSQL Error Code: 57014

++++++++++++++++++++++++++++
ERR_ROCTO_QUERY_TOO_LONG
++++++++++++++++++++++++++++

Text: Query length xxx exceeded maximum size (xxx)

Description/Action: This error indicates that the query length exceeded maximum size. PSQL Error Code: 08P01

++++++++++++++++++++++++
ERR_ROCTO_READ_FAILED
++++++++++++++++++++++++

Text: read failure: xxx

Description/Action: This error is generated when Rocto fails to read data from a remote connection. PSQL Error Code: 08000

+++++++++++++++++++++++++
ERR_ROCTO_READ_TOO_LARGE
+++++++++++++++++++++++++

Text: Read size xxx greater than buffer size xxx

Description/Action: This error indicates that a PostgreSQL wire protocol message exceeded the maximum size of messages which can be read by Rocto. Please contact your YottaDB support channel. PSQL Error Code: 22000

++++++++++++++++++++++++++++++++++++++
INFO_ROCTO_PARAMETER_DESCRIPTION_SENT
++++++++++++++++++++++++++++++++++++++

Text: sent ParameterDescription for prepared statement 'xxx'

Description/Action: This message indicates that a Rocto ParameterDescription message has been sent for a prepared statement. PSQL Error Code: 00000

+++++++++++++++++++++++++++++++++
INFO_ROCTO_PARAMETER_STATUS_SENT
+++++++++++++++++++++++++++++++++

Text: sent ParameterStatus with parameter 'xxx' set to 'xxx'

Description/Action: This message indicates that Rocto recorded the value of a database parameter set by a SET statement, and has notified the client using a ParameterStatus message as part of the PostgreSQL wire protocol startup procedure. PSQL Error Code: 00000

+++++++++++++++++++++++++++++++++++
INFO_ROCTO_ROW_DESCRIPTION_SENT
+++++++++++++++++++++++++++++++++++

Text: sent RowDescription for xxx: 'xxx'

Description/Action: This message indicates that a Rocto RowDescription message has been sent. PSQL Error Code: 00000

+++++++++++++++++++++++++
INFO_ROCTO_SERVER_FORKED
+++++++++++++++++++++++++

Text: rocto server process forked with pid xxx

Description/Action: This message is generated to show the Rocto server fork that is running, along with its PID. PSQL Error Code: 00000

++++++++++++++++++++
INFO_ROCTO_STARTED
++++++++++++++++++++

Text: rocto started on port xxx

Description/Action: This message indicates a successful start of Rocto on the given port. PSQL Error Code: 00000

++++++++++++++++++++++++++++++
ERR_ROCTO_SECRET_KEY_MISMATCH
++++++++++++++++++++++++++++++

Text: handle_cancel_request: secret key/PID pair doesn't match stored value

Description/Action: This error indicates that the secret key/PID pair doesn't match that of the client sending a CancelRequest. PSQL Error Code: 28000

+++++++++++++++++++++++
ERR_ROCTO_SEND_FAILED
+++++++++++++++++++++++

Text: failed to send message of type 'xxx'

Description/Action: This error indicates that Rocto failed to send a message of a specific type to a remote client. PSQL Error Code: 08000

+++++++++++++++++++++++++
ERR_ROCTO_SESSION_LOOKUP
+++++++++++++++++++++++++

Text: xxx: failed to retrieve xxx from session info

This error indicates that Rocto has failed to retrieve the relevant session data for a given client. PSQL Code: XX000

++++++++++++++++++++
ERR_ROCTO_TLS_ACCEPT
++++++++++++++++++++

Text: ydb_tls_accept: xxx

Description/Action: This error indicates that there is an issue with TLS acceptance. PSQL Error Code: XX000

+++++++++++++++++++++++++
ERR_ROCTO_TLS_CONNECTION
+++++++++++++++++++++++++

Text: ydb_tls_get_conn_info: xxx

Description/Action: This error indicates that there is an issue with the TLS connection process. PSQL Error Code: XX000

++++++++++++++++++++
ERR_ROCTO_TLS_INIT
++++++++++++++++++++

Text: ydb_tls_init: xxx

Description/Action: This error indicates that there is an issue with TLS initialization. PSQL Error Code: XX000

++++++++++++++++++++++++++
ERR_ROCTO_TLS_READ_FAILED
++++++++++++++++++++++++++

Text: ydbcrypt: read failed: xxx

Description/Action: This error indicates that an attempt to read from a TLS socket has failed. PSQL Error Code: XX000

+++++++++++++++++++++++
ERR_ROCTO_TLS_REQUIRED
+++++++++++++++++++++++

Text: Server requires all connections to be TLS encrypted. Please re-connect using a client with TLS/SSL enabled.

Description/Action: This error indicates that the server requires all network connections to use TLS encryption, but the client attempted to initiate an unencrypted connection. PSQL Error Code: 08P01

++++++++++++++++++++
ERR_ROCTO_TLS_SOCKET
++++++++++++++++++++

Text: ydb_tls_socket: xxx

Description/Action: This error indicates that there is an issue with the TLS socket. PSQL Error Code: XX000

++++++++++++++++++++++
ERR_ROCTO_TLS_UNKNOWN
++++++++++++++++++++++

Text: ydbcrypt: unknown error: xxx

Description/Action: This error indicates that an unknown TLS error has taken place. PSQL Error Code: XX000

++++++++++++++++++++++
ERR_ROCTO_TLS_UNNAMED
++++++++++++++++++++++

Text: Unnamed failure in ydb_tls_accept: xxx (tls_errno: xxx)

Description/Action: This error indicates that an unnamed TLS error has occurred in the TLS initialization process (accepting a new connection). PSQL Error Code: XX000

++++++++++++++++++++++++
ERR_ROCTO_TLS_WANT_READ
++++++++++++++++++++++++

Text: ydbcrypt: unprocessed read data

Description/Action: This error indicates that there is data remaining to be read from a TLS socket. PSQL Error Code: XX000

+++++++++++++++++++++++++
ERR_ROCTO_TLS_WANT_WRITE
+++++++++++++++++++++++++

Text: ydbcrypt: unprocessed write data

Description/Action: This error indicates that there is data remaining to be written to a TLS socket. PSQL Error Code: XX000

+++++++++++++++++++++++++++
ERR_ROCTO_TLS_WRITE_FAILED
+++++++++++++++++++++++++++

Text: ydbcrypt: write failed: xxx

Description/Action: This error indicates that an attempt to write to a TLS socket has failed. PSQL Error Code: XX000

+++++++++++++++++++++++++
ERR_ROCTO_TOO_FEW_VALUES
+++++++++++++++++++++++++

Text: xxx: too few xxx

Description/Action: This error indicates that a PostgreSQL wire protocol message is missing one or more fields. PSQL Error Code: 22003

++++++++++++++++++++++++++
ERR_ROCTO_TOO_MANY_VALUES
++++++++++++++++++++++++++

Text: xxx: too many xxx

Description/Action: This error indicates that a PostgreSQL wire protocol message was submitted with too many fields. PSQL Error Code: 22003

+++++++++++++++++++++++++
ERR_ROCTO_TRAILING_CHARS
+++++++++++++++++++++++++

Text: xxx: message has trailing characters

Description/Action: The error indicates that a PostgreSQL wire protocol message has trailing characters. PSQL Error Code: 08P01

+++++++++++++++++++++++++++++++++++++
ERR_ROCTO_UNSUPPORTED_BIND_PARAMETER
+++++++++++++++++++++++++++++++++++++

Text: Unsupported bind parameter type received

Description/Action: This error indicates that Rocto has received a request to bind a value of an unsupported data type to a prepared statement. PSQL Error Code: XX000

++++++++++++++++++++++++++++
ERR_ROCTO_USER_LOOKUP
++++++++++++++++++++++++++++

Text: xxx: failed to retrieve xxx for user xxx from database

Description/Action: This error indicates that a client has attempted to log in to Rocto as a non-existent user. PSQL Error Code: 28000

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
