/****************************************************************
 *								*
 * Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

/* Default values for each required runtime setting.
 *
 * Which settings require a default has been determined by testing using various clients and
 * may be subject to change as new clients are tested/supported.
 */
#define DEFAULT_APPLICATION_NAME	    ""
#define DEFAULT_CLIENT_ENCODING		    "SQL_ASCII"
#define DEFAULT_DATESTYLE		    "ISO, MDY"
#define DEFAULT_INTEGER_DATETIMES	    "on"
#define DEFAULT_INTERVALSTYLE		    "postgres"
#define DEFAULT_SERVER_ENCODING		    "SQL_ASCII"
#define DEFAULT_SERVER_VERSION		    "13.0.0"
#define DEFAULT_STANDARD_CONFORMING_STRINGS "on"
#define DEFAULT_TIMEZONE		    "UTC"
/* Note that the following parameters can be SHOWn but not SET in PostgreSQL and are omitted from `pg_settings`.
 * Accordingly, these are handled as special cases.
 */
#define DEFAULT_IS_SUPERUSER	      "on"
#define DEFAULT_SESSION_AUTHORIZATION "postgres"

/* Default row values for each runtime setting known to be required by one or more clients
 *
 * Note that the first column value is omitted in each case for easy construction of row data when initializing and updating
 * runtime variables. Accordingly, this value will be specified either at process startup using the above default values, or else
 * when Octo receives a SET command from a client, which will contain only the value for the parameter but not the rest of the row
 * information.
 */
#define DEFAULT_DATESTYLE_ROW                                                                                                   \
	"||Client Connection Defaults / Locale and Formatting|Sets the display format for date and time values.|Also controls " \
	"interpretation of ambiguous date inputs.|user|string|configuration file||||ISO, MDY|ISO, MDY|||f"
#define DEFAULT_INTERVALSTYLE_ROW                                                                    \
	"||Client Connection Defaults / Locale and Formatting|Sets the display format for interval " \
	"values.||user|enum|default|||{postgres,postgres_verbose,sql_standard,iso_8601}|postgres|postgres|||f"
#define DEFAULT_TIMEZONE_ROW                                                                                            \
	"||Client Connection Defaults / Locale and Formatting|Sets the time zone for displaying and interpreting time " \
	"stamps.||user|string|configuration file||||UTC|UTC|||f"
#define DEFAULT_APPLICATION_NAME_ROW                                                                        \
	"||Reporting and Logging / What to Log|Sets the application name to be reported in statistics and " \
	"logs.||user|string|client||||||||f"
#define DEFAULT_CLIENT_ENCODING_ROW                                                             \
	"||Client Connection Defaults / Locale and Formatting|Sets the client's character set " \
	"encoding.||user|string|default||||SQL_ASCII|SQL_ASCII|||f"
#define DEFAULT_INTEGER_DATETIMES_ROW "||Preset Options|Datetimes are integer based.||internal|bool|default||||on|on|||f"
#define DEFAULT_SERVER_ENCODING_ROW                                                                      \
	"||Client Connection Defaults / Locale and Formatting|Sets the server (database) character set " \
	"encoding.||internal|string|override||||SQL_ASCII|SQL_ASCII|||f"
#define DEFAULT_SERVER_VERSION_ROW                                                                       \
	"||Preset Options|Shows the server version.||internal|string|default||||" DEFAULT_SERVER_VERSION \
	"|" DEFAULT_SERVER_VERSION "|||f"
/* This is an empty row for storing arbitrary runtime parameter values. Since users can specify runtime parameters that have no
 * actual meaning via SET, there may be no valid values for the various columns of pg_settings in that case. Users who wish to
 * specify row values in pg_settings will need to do so explicitly using INSERT or UPDATE.
 */
#define DEFAULT_EMPTY_ROW "|||||||||||||||"
#define DEFAULT_STANDARD_CONFORMING_STRINGS_ROW                                                                          \
	"||Version and Platform Compatibility / Previous PostgreSQL Versions|Causes '...' strings to treat backslashes " \
	"literally.||user|bool|default||||on|on|||f"
