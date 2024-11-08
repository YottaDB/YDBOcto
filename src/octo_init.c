/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <libgen.h>

#include <openssl/conf.h>
#include <openssl/evp.h>
#include <openssl/err.h>

#include "octo.h"
#include "octo_types.h"
#include "rocto_common.h"

#define OCTO_CONF_FILE_NAME "octo.conf"

#define MAX_CONFIG_FILES 3

#define SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(NAME, CARET)                                                               \
	{                                                                                                                       \
		int length;                                                                                                     \
                                                                                                                                \
		length = snprintf(config->global_names.NAME, sizeof(config->global_names.NAME), "%s%%ydbocto%s", CARET, #NAME); \
		if ((0 > length) || ((int)sizeof(config->global_names.NAME) <= length)) {                                       \
			ERROR(ERR_MIDENT_LENGTH, length, YDB_MAX_IDENT);                                                        \
			return 1;                                                                                               \
		}                                                                                                               \
		assert('\0' == config->global_names.NAME[length]);                                                              \
	}

#define MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR(FORMAT, ENV_VAR, CONFIG_FILE, CONFIG_FILE_LIST, FILENAME)                         \
	{                                                                                                                       \
		int tmp_path_len, status;                                                                                       \
                                                                                                                                \
		tmp_path_len = snprintf(FILENAME, OCTO_PATH_MAX, FORMAT, ENV_VAR, OCTO_CONF_FILE_NAME);                         \
		/* Ignore this configuration file if snprintf output was truncated. */                                          \
		if (OCTO_PATH_MAX > tmp_path_len) {                                                                             \
			assert('\0' == FILENAME[tmp_path_len]);                                                                 \
			status = merge_config_file(FILENAME, &CONFIG_FILE, CONFIG_IMPLICIT);                                    \
			if (0 == status) {                                                                                      \
				CONFIG_FILE_LIST.filenames[CONFIG_FILE_LIST.num_files] = FILENAME;                              \
				CONFIG_FILE_LIST.num_files++;                                                                   \
				assert(MAX_CONFIG_FILES >= CONFIG_FILE_LIST.num_files);                                         \
			} else if (1 == status) {                                                                               \
				return 1;                                                                                       \
			} else {                                                                                                \
				/* The file wasn't found or was inaccessible, so don't add file to the list of loaded files. */ \
			}                                                                                                       \
		} else {                                                                                                        \
			return 1;                                                                                               \
		}                                                                                                               \
	}

/* Check for libconfig errors reported after attempting to read a configuration setting. In case of an error, report it and return.
 * Note that this check is only done after another libconfig call failed, and is used to differentiate between the various failure
 * cases of the preceding call. Particularly, this allows us to distinguish syntax errors in a setting from that setting simply
 * being omitted (the CONFIG_ERR_NONE case).
 */
#define CONFIG_ERROR_CHECK(CONFIG_FILE, SETTING_NAME)                                 \
	{                                                                             \
		int error_type;                                                       \
                                                                                      \
		error_type = config_error_type(CONFIG_FILE);                          \
		switch (error_type) {                                                 \
		case CONFIG_ERR_FILE_IO:                                              \
			ERROR(ERR_CONFIG_IO_FAILURE, SETTING_NAME, config_file_name); \
			return 1;                                                     \
			break;                                                        \
		case CONFIG_ERR_PARSE:                                                \
			ERROR(ERR_BAD_CONFIG, config_file_name, SETTING_NAME);        \
			return 1;                                                     \
			break;                                                        \
		default:                                                              \
			assert(CONFIG_ERR_NONE == error_type);                        \
			break;                                                        \
		}                                                                     \
	}

/* Contains a list of names for configuration files and the total number of such files.
 * Used for outputting a list of all configuration files loaded.
 */
typedef struct config_file_list {
	char *filenames[MAX_CONFIG_FILES];
	int   num_files;
} ConfigFileList;

/* Holds the kind of config we are parsing.
 * Used for customizing the behavior of `merge_config_file`.
 */
enum config_kind {
	/* Use Octo's default configuration */
	CONFIG_DEFAULT,
	/* A config that was specified by a `-c` argument */
	CONFIG_EXPLICIT,
	/* A config that was automatically loaded */
	CONFIG_IMPLICIT,
};

int  parse_config_file_settings(const char *config_file_name, config_t *config_file);
void merge_config_file_helper(config_setting_t *a, config_setting_t *b);

/* Returns 0 for success, 1 on error, and 2 for file not found/inaccessible (not technically an error, but must be distinguished for
 * so the caller only issues a config loaded message when the file is actually used)
 * The is_default flag is used to indicate that the default configuration should be merged in. This is needed since the default
 * config is read from a string, not a file, so this case must be treated separately.
 */
int merge_config_file(const char *path, config_t **config_file, enum config_kind kind) {
	config_setting_t *a_root, *b_root;
	config_t	 *new_config_file;
	const char	 *error_message, *error_file;
	int		  error_line, status;

	new_config_file = (config_t *)calloc(1, sizeof(config_t));
	config_init(new_config_file);
	if (CONFIG_DEFAULT != kind) {
		assert(NULL != path);
		if (-1 == access(path, F_OK)) {
			// File not found or no access; skip it
			if (CONFIG_EXPLICIT == kind) {
				// This file was explicitly requested, so give an error if it's not found
				ERROR(ERR_FILE_NOT_FOUND, path);
			}
			CLEANUP_CONFIG(new_config_file);
			return 2;
		}
		if (CONFIG_FALSE == config_read_file(new_config_file, path)) {
			error_message = config_error_text(new_config_file);
			error_file = config_error_file(new_config_file);
			error_line = config_error_line(new_config_file);
			ERROR(ERR_PARSING_CONFIG, error_file, error_line, error_message);
			CLEANUP_CONFIG(new_config_file);
			return 1;
		}
	} else {
		// TODO: this leaks memory for the same reason as `parse_config_file_settings` (see below).
		ssize_t	    exe_path_len;
		char	    exe_path[OCTO_PATH_MAX], default_conf_file[OCTO_PATH_MAX];
		char	   *ydb_dist;
		const char *src_path;

		if (DISABLE_INSTALL) {
			exe_path_len = readlink("/proc/self/exe", exe_path, OCTO_PATH_MAX);
			if ((-1 != exe_path_len) && (OCTO_PATH_MAX > exe_path_len)) {
				exe_path[exe_path_len] = '\0'; // readlink() doesn't add a null terminator per man page
				src_path = dirname(exe_path);
				if (NULL != src_path) {
					status = snprintf(default_conf_file, sizeof(default_conf_file), "%s/octo.conf", src_path);
					if ((int)sizeof(default_conf_file) <= status) {
						ERROR(ERR_BUFFER_TOO_SMALL, "octo.conf file path");
						return 1;
					}
				} else {
					ERROR(ERR_LIBCALL_WITH_ARG, "dirname()", exe_path);
					return 1;
				}
			} else {
				ERROR(ERR_LIBCALL_WITH_ARG, "readlink()", "/proc/self/exe");
				return 1;
			}
		} else {
			ydb_dist = getenv("ydb_dist");
			if (NULL == ydb_dist) {
				ERROR(ERR_FAILED_TO_RETRIEVE_ENVIRONMENT_VARIABLE, "ydb_dist");
				return 1;
			}
			status = snprintf(default_conf_file, sizeof(default_conf_file), "%s/plugin/octo/octo.conf", ydb_dist);
			if ((int)sizeof(default_conf_file) <= status) {
				ERROR(ERR_BUFFER_TOO_SMALL, "octo.conf file path");
				return 1;
			}
		}
		if (CONFIG_FALSE == config_read_file(new_config_file, default_conf_file)) {
			error_message = config_error_text(new_config_file);
			error_file = "default";
			error_line = config_error_line(new_config_file);
			ERROR(ERR_PARSING_CONFIG, error_file, error_line, error_message);
			CLEANUP_CONFIG(new_config_file);
			return 1;
		}
		path = "default"; // Use literal in place of file path when issuing errors in parse_config_file_settings
	}
	a_root = config_root_setting(*config_file);
	b_root = config_root_setting(new_config_file);
	merge_config_file_helper(b_root, a_root);
	CLEANUP_CONFIG(*config_file);
	*config_file = new_config_file;
	// TODO: this function leaks memory. See !656 for ideas on how to fix it, and why it will be hard.
	status = parse_config_file_settings(path, *config_file);
	return status;
}

// Merges b into a, updating a with any values from b
void merge_config_file_helper(config_setting_t *a, config_setting_t *b) {
	config_setting_t *t_setting, *b_setting;
	char		 *setting_name;
	int		  setting_type, b_index;

	b_index = 0;
	while (TRUE) {
		b_setting = config_setting_get_elem(b, b_index);
		if (NULL == b_setting)
			break;
		setting_name = config_setting_name(b_setting);
		setting_type = config_setting_type(b_setting);
		if (config_setting_is_group(b_setting) || config_setting_is_array(b_setting) || config_setting_is_list(b_setting)) {
			switch (config_setting_type(a)) {
			case CONFIG_TYPE_GROUP:
				t_setting = config_setting_get_member(a, setting_name);
				break;
			case CONFIG_TYPE_ARRAY:
			case CONFIG_TYPE_LIST:
			default:
				t_setting = NULL;
				break;
			}
			if (NULL == t_setting) {
				t_setting = config_setting_add(a, setting_name, setting_type);
			}
			merge_config_file_helper(t_setting, b_setting);
		} else {
			config_setting_remove(a, setting_name);
			t_setting = config_setting_add(a, setting_name, setting_type);
			switch (config_setting_type(b_setting)) {
			case CONFIG_TYPE_STRING:
				config_setting_set_string(t_setting, config_setting_get_string(b_setting));
				break;
			case CONFIG_TYPE_INT:
				config_setting_set_int(t_setting, config_setting_get_int(b_setting));
				break;
			case CONFIG_TYPE_INT64:
				config_setting_set_int64(t_setting, config_setting_get_int64(b_setting));
				break;
			case CONFIG_TYPE_FLOAT:
				config_setting_set_float(t_setting, config_setting_get_float(b_setting));
				break;
			case CONFIG_TYPE_BOOL:
				config_setting_set_bool(t_setting, config_setting_get_bool(b_setting));
				break;
			}
		}
		b_index++;
	}
}

int parse_config_file_settings(const char *config_file_name, config_t *config_file) {
	config_setting_t *ydb_settings, *cur_ydb_setting;
	ydb_buffer_t	  zroutines_buffer, dollar_zroutines_buffer;
	unsigned int	  offset, zroutines_from_file_len, zroutines_len;
	const char	 *item_name, *item_value, *verbosity, *tabletype, *emulate;
	char		 *zroutines_buf_start, *zroutines_from_file;
	int		  status, done, i, emulate_int, verbosity_int, tabletype_int, plan_src_dir_len, plan_obj_dir_len;
	char		  plan_src_dir[OCTO_PATH_MAX], plan_obj_dir[OCTO_PATH_MAX], *obj_dir;
	struct stat	  statbuf;

	if (CONFIG_TRUE == config_lookup_string(config_file, "verbosity", &verbosity)) {
		if (strcmp(verbosity, "TRACE") == 0) {
			verbosity_int = TRACE;
		} else if (strcmp(verbosity, "DEBUG") == 0) {
			verbosity_int = DEBUG;
		} else if (strcmp(verbosity, "INFO") == 0) {
			verbosity_int = INFO;
		} else if (strcmp(verbosity, "ERROR") == 0) {
			verbosity_int = ERROR;
		} else {
			ERROR(ERR_BAD_CONFIG, config_file_name,
			      "verbosity can only take on string values TRACE, DEBUG, INFO or ERROR");
			return 1;
		}
	} else if (CONFIG_FALSE == config_lookup_int(config_file, "verbosity", &verbosity_int)) {
		CONFIG_ERROR_CHECK(config_file, "verbosity");
		verbosity_int = ERROR; // Set to the default if verbosity was not set, i.e. the error check succeeds
	} else {
		ERROR(ERR_BAD_CONFIG, config_file_name, "verbosity can only take on string values (not integer values)");
		return 1;
	}
	config->verbosity_level = verbosity_int;
	if (CONFIG_TRUE == config_lookup_string(config_file, "emulate", &emulate)) {
		if (strcmp(emulate, "POSTGRES") == 0) {
			emulate_int = POSTGRES;
		} else if (strcmp(emulate, "MYSQL") == 0) {
			emulate_int = MYSQL;
		} else {
			ERROR(ERR_BAD_CONFIG, config_file_name, "'emulate' can only take on string values 'POSTGRES' and 'MYSQL'");
			return 1;
		}
	} else if (CONFIG_FALSE == config_lookup_int(config_file, "emulate", &emulate_int)) {
		CONFIG_ERROR_CHECK(config_file, "emulate");
		emulate_int = POSTGRES; // Set to the default if no emulation was specified
	} else {
		ERROR(ERR_BAD_CONFIG, config_file_name, "'emulate' can only take on string values 'POSTGRES' and 'MYSQL'");
		return 1;
	}
	config->database_emulation = emulate_int;
	if (CONFIG_FALSE == config_lookup_string(config_file, "rocto.address", &config->rocto_config.address)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.address");
	}
	if (CONFIG_FALSE == config_lookup_int(config_file, "rocto.port", &config->rocto_config.port)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.port");
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.use_dns", &config->rocto_config.use_dns)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.use_dns");
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.tcp_delay", &config->rocto_config.tcp_delay)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.tcp_delay");
	}
#if YDB_TLS_AVAILABLE
	if (CONFIG_FALSE == config_lookup_string(config_file, "tls.OCTOSERVER.cert", &config->rocto_config.ssl_cert_file)) {
		CONFIG_ERROR_CHECK(config_file, "tls.OCTOSERVER.cert");
	}
	if (CONFIG_FALSE == config_lookup_string(config_file, "tls.OCTOSERVER.key", &config->rocto_config.ssl_key_file)) {
		CONFIG_ERROR_CHECK(config_file, "tls.OCTOSERVER.key");
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.ssl_on");
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.ssl_required", &config->rocto_config.ssl_required)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.ssl_required");
	}
	if (!config->rocto_config.ssl_on && config->rocto_config.ssl_required) {
		ERROR(ERR_BAD_CONFIG, config_file_name, "rocto.ssl_required set, but rocto.ssl_on is disabled");
		return 1;
	}

#else
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.ssl_on");
	}
	if (config->rocto_config.ssl_on) {
		ERROR(ERR_BAD_CONFIG, config_file_name, "rocto.ssl_on set, but YottaDB TLS plugin not installed");
		return 1;
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.ssl_required", &config->rocto_config.ssl_required)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.ssl_required");
	}
	if (config->rocto_config.ssl_required) {
		ERROR(ERR_BAD_CONFIG, config_file_name, "rocto.ssl_required set, but YottaDB TLS plugin not installed");
		return 1;
	}
#endif
	// Read in YDB settings
	ydb_settings = config_lookup(config_file, "yottadb");
	if (ydb_settings != NULL && config_setting_is_group(ydb_settings) == CONFIG_TRUE) {
		i = 0;
		while ((cur_ydb_setting = config_setting_get_elem(ydb_settings, i)) != NULL) {
			item_name = config_setting_name(cur_ydb_setting);
			item_value = config_setting_get_string(cur_ydb_setting);
			if (NULL == getenv(item_name)) {
				setenv(item_name, item_value, 0);
			}
			i++;
		}
	}

	/* Read in "tabletype" setting */
	status = config_lookup_string(config_file, "tabletype", &tabletype);
	if (CONFIG_TRUE == status) {
		/* Check if a valid value is specified. If not issue error. */
		if (0 == strcmp(tabletype, "READONLY")) {
			config->default_tabletype = TABLETYPE_READONLY;
		} else if (0 == strcmp(tabletype, "READWRITE")) {
			config->default_tabletype = TABLETYPE_READWRITE;
		} else {
			ERROR(ERR_BAD_CONFIG, config_file_name, "tabletype can only take on string values READONLY or READWRITE");
			return 1;
		}
	} else if (CONFIG_FALSE == config_lookup_int(config_file, "tabletype", &tabletype_int)) {
		UNUSED(tabletype_int);
		CONFIG_ERROR_CHECK(config_file, "tabletype");
		config->default_tabletype = TABLETYPE_READWRITE; /* default tabletype for CREATE TABLE is READWRITE */
	} else {
		ERROR(ERR_BAD_CONFIG, config_file_name, "tabletype can only take on string values (not integer values)");
		return 1;
	}
#ifndef NDEBUG
	/* Read in "seedreload" setting */
	/* Only used for testing */
	const char *seedreload;
	status = config_lookup_string(config_file, "seedreload", &seedreload);
	if (CONFIG_TRUE == status) {
		/* Check if a valid value is specified. If not issue error. */
		if (0 == strcmp(seedreload, "TRUE")) {
			config->seedreload = TRUE;
		} else {
			config->seedreload = FALSE;
		}
	} else {
		config->seedreload = FALSE;
	}
#endif
	/* Read in "datestyle" setting */
	const char *datestyle;
	status = config_lookup_string(config_file, "datestyle", &datestyle);
	if (CONFIG_TRUE == status) {
		char *datestyle_value = malloc(sizeof(char) * strlen(datestyle) + 1);
		strcpy(datestyle_value, datestyle);
		status = set_date_time_format_from_datestyle(datestyle_value);
		free(datestyle_value);
		if (0 != status) {
			ERROR(ERR_BAD_CONFIG, config_file_name, "datestyle value is incorrect in config");
			return 1;
		}
	} else {
		// Default is set here instead of after load_pg_defaults as auto-upgrade procedure needs these defaults
		char *datestyle_value = malloc(sizeof(char) * strlen(DEFAULT_DATESTYLE) + 1);
		if (POSTGRES == config->database_emulation) {
			strcpy(datestyle_value, OCTO_DEFAULT_POSTGRES_DATESTYLE);
		} else {
			strcpy(datestyle_value, OCTO_DEFAULT_MYSQL_DATESTYLE);
		}
		status = set_date_time_format_from_datestyle(datestyle_value);
		free(datestyle_value);
		assert(!status);
		UNUSED(status); // Prevent clang-analyzer-deadcode.DeadStores warning
	}
	/* Read in "outputformat" setting */
	const char *outputformat;
	status = config_lookup_string(config_file, "datetimeoutputformat", &outputformat);
	if (CONFIG_TRUE == status) {
		/* Check if a valid value is specified. If not issue error. */
		if (0 == strcmp(outputformat, OCTOLIT_DATE_TIME_HOROLOG)) {
			config->date_time_output_format = OPTIONAL_DATE_TIME_HOROLOG;
		} else if (0 == strcmp(outputformat, OCTOLIT_DATE_TIME_ZHOROLOG)) {
			config->date_time_output_format = OPTIONAL_DATE_TIME_ZHOROLOG;
		} else if (0 == strcmp(outputformat, OCTOLIT_DATE_TIME_ZUT)) {
			config->date_time_output_format = OPTIONAL_DATE_TIME_ZUT;
		} else if (0 == strcmp(outputformat, OCTOLIT_DATE_TIME_FILEMAN)) {
			config->date_time_output_format = OPTIONAL_DATE_TIME_FILEMAN;
		} else if (0 == strcmp(outputformat, OCTOLIT_DATE_TIME_TEXT)) {
			config->date_time_output_format = OPTIONAL_DATE_TIME_TEXT;
		} else {
			ERROR(ERR_BAD_CONFIG, config_file_name,
			      "output format is incorrect, it can be horolog,zhorolog,zut,fileman or text");
			return 1;
		}
	} else {
		config->date_time_output_format = OPTIONAL_DATE_TIME_TEXT;
	}
	// Set LVN such that plans can make use of this instead of hashing this value
	ydb_buffer_t ydboctodatetimeoutputformat;
	YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTODATETIMEOUTPUTFORMAT, &ydboctodatetimeoutputformat);
	ydb_buffer_t val;
	char	     numbuf[INT32_TO_STRING_MAX + 1];
	val.buf_addr = numbuf;
	val.len_alloc = sizeof(numbuf);
	val.len_used = snprintf(val.buf_addr, val.len_alloc, "%d", config->date_time_output_format);
	status = ydb_set_s(&ydboctodatetimeoutputformat, 0, NULL, &val);
	assert(YDB_OK == status);
	UNUSED(status); /* needed to avoid [-Wunused-but-set-variable] warning from compiler */

	/* Load octo_history and octo_history_max_length
	 * For octo_history if not set, it will just be NULL, which we can easily
	 * check.
	 * if octo_history_max_length is not set, we need to set it to -1, as 0 is
	 * a valid value and we cannot tell whether a user entered zero or if a
	 * config value was not found.
	 */
	if (CONFIG_FALSE == config_lookup_string(config_file, "octo_history", &config->octo_history))
		CONFIG_ERROR_CHECK(config_file, "octo_history");
	if (CONFIG_FALSE == config_lookup_int(config_file, "octo_history_max_length", &config->octo_history_max_length)) {
		config->octo_history_max_length = OCTO_HISTORY_MAX_LENGTH_UNSET;
		CONFIG_ERROR_CHECK(config_file, "octo_history_max_length")
	}

	// $ZROUTINES has already been set, just return now
	if (NULL != config->plan_src_dir) {
		return 0;
	}
	zroutines_len = ZRO_INIT_ALLOC;
	YDB_MALLOC_BUFFER(&zroutines_buffer, zroutines_len);
	if (NULL == zroutines_buffer.buf_addr) {
		ERROR(ERR_SYSCALL, "YDB_MALLOC_BUFFER", errno, strerror(errno));
		return 1;
	}
	/* We need space at the end of "zroutines_buffer" to store a ' ' and '\0' (as this is needed by the "while" loop
	 * that is invoked later in this function to set "config->plan_src_dir". Hence reduce len_alloc by 2 bytes.
	 */
	zroutines_buffer.len_alloc -= 2;
	YDB_LITERAL_TO_BUFFER("$zroutines", &dollar_zroutines_buffer);
	// Prepend zroutines path from configuration file to the start of $zroutines, if specified.
	if (CONFIG_TRUE == config_lookup_string(config_file, "octo_zroutines", (const char **)&zroutines_from_file)) {
		zroutines_from_file_len = strlen(zroutines_from_file);
		YDB_COPY_STRING_TO_BUFFER(zroutines_from_file, &zroutines_buffer, done);
		// Double size if ZRO_INIT_ALLOC is too small to avoid potential resize later.
		if (!done) {
			YDB_FREE_BUFFER(&zroutines_buffer);
			zroutines_len = (zroutines_from_file_len * 2) + 2; /* "* 2" to double size, "+ 2" for ' ' and '\0' at end */
			YDB_MALLOC_BUFFER(&zroutines_buffer, zroutines_len);
			if (NULL == zroutines_buffer.buf_addr) {
				ERROR(ERR_SYSCALL, "YDB_MALLOC_BUFFER", errno, strerror(errno));
				return 1;
			}
			YDB_COPY_STRING_TO_BUFFER(zroutines_from_file, &zroutines_buffer, done);
			assert(done);
		}
		do {
			// Shift start of buffer to after the zroutines path pulled from the configuration file
			zroutines_buf_start = zroutines_buffer.buf_addr;
			zroutines_buffer.buf_addr[zroutines_from_file_len] = ' ';
			zroutines_buffer.buf_addr = zroutines_buf_start + zroutines_from_file_len + 1;
			assert(zroutines_len >= (zroutines_from_file_len + 2));
			zroutines_buffer.len_alloc = zroutines_len - zroutines_from_file_len - 2; // ' ' + '\0'
			zroutines_buffer.len_used = 0;
			// Retrieve value of $zroutines
			status = ydb_get_s(&dollar_zroutines_buffer, 0, NULL, &zroutines_buffer);
			if (YDB_ERR_INVSTRLEN == status) {
				/* If the buffer isn't large enough allocate more and call ydb_get_s() again
				 * + 1 for space after the zroutines read from the octo.conf file.
				 * + 2 for space and null terminator after the zroutines read from ydb_routines env var.
				 */
				zroutines_len = zroutines_buffer.len_used + 1 + zroutines_from_file_len + 2;
				zroutines_buffer.buf_addr = zroutines_buf_start;
				YDB_FREE_BUFFER(&zroutines_buffer);
				YDB_MALLOC_BUFFER(&zroutines_buffer, zroutines_len);
				if (NULL == zroutines_buffer.buf_addr) {
					ERROR(ERR_SYSCALL, "YDB_MALLOC_BUFFER", errno, strerror(errno));
					return 1;
				}
				YDB_COPY_STRING_TO_BUFFER(zroutines_from_file, &zroutines_buffer, done);
				assert(done);
			} else {
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					zroutines_buffer.buf_addr = zroutines_buf_start;
					YDB_FREE_BUFFER(&zroutines_buffer);
					return 1;
				}
			}
		} while (YDB_OK != status);

		// Back the pointer up to the start of the buffer
		zroutines_buffer.buf_addr = zroutines_buf_start;
		zroutines_buffer.len_used += zroutines_from_file_len + 1; // Space character ' '
		zroutines_buffer.len_alloc = zroutines_len;
		assert(zroutines_buffer.len_used < zroutines_buffer.len_alloc); /* "<" guaranteed (no need of "<=") since we
										 * reserved space for '\0' at end.
										 */
		status = ydb_set_s(&dollar_zroutines_buffer, 0, NULL, &zroutines_buffer);
		if (YDB_ERR_ZROSYNTAX == status || YDB_ERR_DLLNOOPEN == status) {
			YDB_ERROR_CHECK(status);
			ERROR(ERR_BAD_CONFIG, config_file_name, "octo_zroutines");
			YDB_FREE_BUFFER(&zroutines_buffer);
			return 1;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&zroutines_buffer);
			return 1;
		}
		assert(zroutines_buffer.len_alloc >= zroutines_buffer.len_used + 1);
		zroutines_buffer.buf_addr[zroutines_buffer.len_used] = '\0'; // Null terminate env var
		setenv("ydb_routines", zroutines_buffer.buf_addr, TRUE);
	} else {
		// Otherwise just get $zroutines
		status = ydb_get_s(&dollar_zroutines_buffer, 0, NULL, &zroutines_buffer);
		// If ZRO_INIT_ALLOC isn't large enough allocate more and call ydb_get_s() again
		if (YDB_ERR_INVSTRLEN == status) {
			zroutines_len = zroutines_buffer.len_used + 2; // Null terminator plus padding space
			YDB_FREE_BUFFER(&zroutines_buffer);
			YDB_MALLOC_BUFFER(&zroutines_buffer, zroutines_len);
			if (NULL == zroutines_buffer.buf_addr) {
				ERROR(ERR_SYSCALL, "YDB_MALLOC_BUFFER", errno, strerror(errno));
				return 1;
			}
			status = ydb_get_s(&dollar_zroutines_buffer, 0, NULL, &zroutines_buffer);
		} else {
			/* Restore "len_alloc" to what it originally was (i.e. undo "-= 2" done in a prior step).
			 * This is relied upon by a later "assert(zroutines_buffer.len_used <= zroutines_buffer.len_alloc);".
			 */
			zroutines_buffer.len_alloc += 2;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(&zroutines_buffer);
			return 1;
		}
	}
	zroutines_buf_start = zroutines_buffer.buf_addr;

	// Trim leading white space
	offset = 0;
	status = TRUE;
	while (status && (offset < zroutines_buffer.len_used)) {
		switch (zroutines_buffer.buf_addr[offset]) {
		case ' ':
			offset++;
			break;
		default:
			status = FALSE;
			break;
		}
	}
	zroutines_buffer.buf_addr += offset;
	zroutines_buffer.len_used -= offset;
	// Pad end of zroutines with a space to trigger the switch case
	zroutines_buffer.buf_addr[zroutines_buffer.len_used] = ' ';
	zroutines_buffer.buf_addr[zroutines_buffer.len_used + 1] = '\0';
	zroutines_buffer.len_used += 2;
	assert(zroutines_buffer.len_used <= zroutines_buffer.len_alloc);

	// Extract the leading path and store in config->plan_src_dir
	offset = 0;
	obj_dir = NULL;
	while ('\0' != zroutines_buffer.buf_addr[offset]) {
		switch (zroutines_buffer.buf_addr[offset]) {
		// White space characters and right parenthesis are delimiters
		case ' ':
		case ')':
			// Terminate everything read up to this point, if last character is a '*' remove it
			if (1 < offset) {
				if ('*' == zroutines_buffer.buf_addr[offset - 1]) {
					offset--;
				}
			}
			// The current path string is the empty string, skip and continue.
			if (0 == offset) {
				/* If the next character (offset+1) is the end of the buffer, the loop will break on '\0'
				 * and an error will be issued below for an invalid zroutines source path (empty string).
				 */
				zroutines_buffer.buf_addr += (offset + 1);
				break;
			}
			// Check if this path is a directory and, if not, move the start of the string to the next token
			zroutines_buffer.buf_addr[offset] = '\0';
			status = stat(zroutines_buffer.buf_addr, &statbuf);
			// If file doesn't exist, don't emit an error and skip it
			if ((0 != status) && (ENOTDIR != errno)) {
				ERROR(ERR_SYSCALL_WITH_ARG, "stat", errno, strerror(errno), zroutines_buffer.buf_addr);
				zroutines_buffer.buf_addr = zroutines_buf_start;
				YDB_FREE_BUFFER(&zroutines_buffer);
				return 1;
			}
			if (!S_ISDIR(statbuf.st_mode)) {
				zroutines_buffer.buf_addr += (offset + 1);
				offset = 0;
				break;
			}
			/* Store the absolute directory path of source directory (where M plans will be generated) as that
			 * is later needed by "get_full_path_of_generated_m_file()".
			 */
			assert(PATH_MAX <= sizeof(plan_src_dir)); /* needed by "realpath()" call below */
			if (NULL == realpath(zroutines_buffer.buf_addr, plan_src_dir)) {
				status = errno;
				ERROR(ERR_SYSCALL_WITH_ARG, "realpath(srcdir)", status, strerror(status),
				      zroutines_buffer.buf_addr);
				zroutines_buffer.buf_addr = zroutines_buf_start;
				YDB_FREE_BUFFER(&zroutines_buffer);
				return 1;
			}
			plan_src_dir_len = strlen(plan_src_dir);
			config->plan_src_dir = malloc(plan_src_dir_len + 1); /* + 1 for null terminator */
			/* cast to char* to avoid memcpy warning (since config->plan_src_dir is const pointer) */
			memcpy((char *)config->plan_src_dir, plan_src_dir, plan_src_dir_len + 1);
			if (NULL != obj_dir) {
				/* Now that we have figured out the source directory where M plans are going to be generated,
				 * store the absolute directory path of corresponding object directory (where .o files
				 * corresponding to M plans will be generated). This is so when we need to delete the .m files
				 * (e.g. DISCARD ALL etc.), we also know where to find the .o files and delete them too.
				 */
				assert(PATH_MAX <= sizeof(plan_obj_dir)); /* needed by "realpath()" call below */
				if (NULL == realpath(obj_dir, plan_obj_dir)) {
					status = errno;
					ERROR(ERR_SYSCALL_WITH_ARG, "realpath(objdir)", status, strerror(status), obj_dir);
					zroutines_buffer.buf_addr = zroutines_buf_start;
					YDB_FREE_BUFFER(&zroutines_buffer);
					return 1;
				}
				plan_obj_dir_len = strlen(plan_obj_dir);
				config->plan_obj_dir = malloc(plan_obj_dir_len + 1); /* + 1 for null terminator */
				/* cast to char* to avoid memcpy warning (since config->plan_obj_dir is const pointer) */
				memcpy((char *)config->plan_obj_dir, plan_obj_dir, plan_obj_dir_len + 1);
			} else {
				/* In this case, the object directory is the same as the source directory.
				 * (e.g. set $zroutines="dir" implies "dir" is the object and source directory).
				 */
				config->plan_obj_dir = config->plan_src_dir;
			}
			break;
		case '(':
			/* If a '(' is found, we parsed past the object directory. Now comes the list of source directories.
			 * Shift start of string. But before that, take a note of this in case we find our source directory here.
			 */
			obj_dir = zroutines_buffer.buf_addr;
			zroutines_buffer.buf_addr += (offset + 1);
			if ((1 < offset) && ('*' == obj_dir[offset - 1])) {
				/* If this object directory has auto relink specified ('*' would be last character),
				 * then ignore that for objdir name purposes.
				 */
				offset--;
			}
			obj_dir[offset] = '\0'; /* NULL terminate the object directory for later use */
			offset = 0;
			break;
		default:
			offset++;
			break;
		}
	}
	zroutines_buffer.buf_addr = zroutines_buf_start;
	YDB_FREE_BUFFER(&zroutines_buffer);
	if (NULL == config->plan_src_dir) {
		ERROR(ERR_BAD_ZROUTINES, NULL);
		return 1;
	}

	return 0;
}

int populate_global_names(void) {
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(schema, "^");	 /* ^%ydboctoschema */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(session, "");	 /*  %ydboctosession */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(cursor, "");	 /*  %ydboctocursor */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(octo, "^");	 /* ^%ydboctoocto */
	config->global_names.raw_octo = &config->global_names.octo[1];	 /*  %ydboctoocto */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(xref, "^");	 /* ^%ydboctoxref */
	config->global_names.raw_xref = &config->global_names.xref[1];	 /*  %ydboctoxref */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(loadedschemas, ""); /*  %ydboctoloadedschemas */
	return 0;
}

// TODO: this function leaks about half a kilobyte of memory
void init_crypto(void) {
	/* Load the human readable error strings for libcrypto */
	ERR_load_crypto_strings();

	/* Load all digest and cipher algorithms */
	OpenSSL_add_all_algorithms();

	/* Load default config file, and other important initialisation */
	CONF_modules_load_file(NULL, NULL, 0);
}

int octo_init(int argc, char **argv) {
	OctoConfig     temp_config;
	const char    *src_path;
	ydb_long_t     ci_return;
	uintptr_t      ci_tab_handle_new, ci_tab_handle_old;
	boolean_t      verbosity_set;
	config_t      *config_file;
	ssize_t	       exe_path_len;
	char	       ci_path[OCTO_PATH_MAX], exe_path[OCTO_PATH_MAX], cwd[OCTO_PATH_MAX], xc_path[OCTO_PATH_MAX];
	char	       cwd_file_name[OCTO_PATH_MAX], homedir_file_name[OCTO_PATH_MAX], plugin_file_name[OCTO_PATH_MAX];
	char	       zstatus_message[YDB_MAX_ERRORMSG];
	char	      *homedir, *ydb_dist, *most_recent_filename, *config_file_name = NULL;
	int	       status, i;
	DIR	      *dir;
	ConfigFileList config_file_list;

	config = (OctoConfig *)malloc(sizeof(OctoConfig));
	memset(config, 0, sizeof(OctoConfig));
	config->config_file = (config_t *)calloc(1, sizeof(config_t));
	config_file = config->config_file;

	// If OCTO_SETTINGS env is set, we previously parsed the config and stashed it in environment variables; load from there

	// Parse the startup flags; we will have to do this again after we read the config
	status = parse_startup_flags(argc, argv, &config_file_name);
	if (status) {
		return status;
	}
	/* Store initial startup flags for later overwriting configuration file settings
	 * No need to track --dry_run or --allow_schema_changes, since these are only
	 * command line options and have no corresponding settings in configuration files.
	 */
	if (ERROR >= config->verbosity_level) {
		temp_config.verbosity_level = config->verbosity_level;
		verbosity_set = TRUE;
	} else {
		/* This line is here to prevent incorrect -Wmaybe-uninitialized compiler warnings
		 * in RelWithDebInfo builds on some platforms.
		 */
		temp_config.verbosity_level = ERROR;
		verbosity_set = FALSE;
	}
	temp_config.rocto_config.port = config->rocto_config.port;
	temp_config.database_emulation = config->database_emulation;
	/* Set initial verbosity to the default to rollback the verbosity_unset case in parse_startup_flags and
	 * allow errors during config merging to be reported
	 */
	config->verbosity_level = ERROR;

	// Search for config file octo.conf (OCTO_CONF_FILE_NAME) in directories ".", "~", and "$ydb_dist/plugin/octo" in that order
	config_init(config_file);

	/* At octo/rocto process startup, the default signal handler (SIG_DFL) for SIGTERM would be established which would
	 * cause a MUPIP STOP that is sent to an octo/rocto process to terminate the process immediately (due to YDB@ae2721d8).
	 * That would leave semaphores/shared-memory-segments resulting in an ipc leak which eventually builds up and results in
	 * the system running out of ipc space (e.g. if one runs the entire bats test). To avoid such a situation, change SIG_DFL
	 * to SIG_IGN that way we ensure when the YottaDB SIGTERM signal handler gets control, it does not immediately terminate
	 * the process but waits for a logical point to do it (and more importantly takes care of running down the databases and
	 * cleaning up the ipcs).
	 */
	struct sigaction act;

	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	act.sa_handler = SIG_IGN;
	sigaction(SIGTERM, &act, NULL);

	// Note: This loop is only ever executed once. Structured this way to make it easy to break out in case of error code paths.
	for (;;) {
		ydb_buffer_t zyrelease, zyrel_value;
		char	    *ch, *ch2, zyrel_value_buff[128]; /* should be more than enough to store $ZYRELEASE value */

		// This should always be 1
		setenv("ydb_lvnullsubs", "1", 1);
		/* This should always be 0 because we check for YDB_ERR_GVUNDEF and YDB_ERR_LVUNDEF in various places in
		Octo and having ydb_noundef set to 1 causes these two error codes to never be returned resulting in a very
		different code flow that can end up in assert failures and/or SIG-11s.
		See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/1026 for more details.*/
		setenv("ydb_noundef", "0", 1);
		status = ydb_init();
		if (YDB_OK != status) {
			ydb_zstatus(zstatus_message, sizeof(zstatus_message));
			ERROR(ERR_YOTTADB, zstatus_message);
			status = 1;
			break;
		}

		// Load config file
		config_file_list.num_files = 0;
		if (NULL == config_file_name) {
			if (NULL == getcwd(cwd, sizeof(cwd))) {
				ERROR(ERR_SYSCALL, "getcwd", errno, strerror(errno));
				status = 1;
				break;
			}
			MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR("%s/%s", cwd, config_file, config_file_list, cwd_file_name);
			homedir = getenv("HOME");
			if (NULL != homedir) {
				MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR("%s/%s", homedir, config_file, config_file_list,
								      homedir_file_name);
			}
			ydb_dist = getenv("ydb_dist");
			if (NULL != ydb_dist) {
				MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR("%s/plugin/octo/%s", ydb_dist, config_file, config_file_list,
								      plugin_file_name);
			}
		} else {
			status = merge_config_file(config_file_name, &config_file, CONFIG_EXPLICIT);
			if (0 != status) {
				status = 1;
				break;
			}
			config_file_list.filenames[config_file_list.num_files++] = config_file_name;
			assert(MAX_CONFIG_FILES >= config_file_list.num_files);
		}
		// Load the default config to populate any empty fields with default values
		status = merge_config_file(NULL, &config_file, CONFIG_DEFAULT);
		if (0 != status) {
			status = 1;
			break;
		}
		if (0 == config_file_list.num_files) {
			most_recent_filename = "default";
		} else {
			most_recent_filename = config_file_list.filenames[config_file_list.num_files - 1];
		}
		status = parse_config_file_settings(most_recent_filename, config_file);
		if (status) {
			break;
		}

		// Apply startup flags from initial parse to overwrite values from config files
		if (verbosity_set) { // Only overwrite if initialized
			config->verbosity_level = temp_config.verbosity_level;
		}
		if (-1 != temp_config.rocto_config.port) { // Only overwrite if initialized
			config->rocto_config.port = temp_config.rocto_config.port;
		}
		if (EMULATION_UNSET != temp_config.database_emulation) { // Only overwrite if initialized
			config->database_emulation = temp_config.database_emulation;
			char *datestyle_value = malloc(sizeof(char) * strlen(DEFAULT_DATESTYLE) + 1);
			if (POSTGRES == config->database_emulation) {
				strcpy(datestyle_value, OCTO_DEFAULT_POSTGRES_DATESTYLE);
			} else {
				strcpy(datestyle_value, OCTO_DEFAULT_MYSQL_DATESTYLE);
			}
			status = set_date_time_format_from_datestyle(datestyle_value);
			free(datestyle_value);
			assert(!status);
			UNUSED(status); // Prevent clang-analyzer-deadcode.DeadStores warning
		}
		// Issue INFO messages for loaded configuration files now that verbosity level is finalized
		for (i = 0; i < config_file_list.num_files; i++) {
			assert(NULL != config_file_list.filenames[i]);
			INFO(INFO_LOADED_CONFIG, config_file_list.filenames[i]);
		}

		// Verify that the directory exists, or issue an error
		dir = opendir(config->plan_src_dir);
		if (NULL == dir) {
			ERROR(ERR_SYSCALL_WITH_ARG, "opendir(config.plan_src_dir)", errno, strerror(errno), config->plan_src_dir);
			status = 1;
			break;
		}
		status = closedir(dir);
		if (0 != status) {
			ERROR(ERR_SYSCALL_WITH_ARG, "closedir(config.plan_src_dir)", errno, strerror(errno), config->plan_src_dir);
			status = 1;
			break;
		}

		config->page_size = sysconf(_SC_PAGESIZE);
		status = populate_global_names();
		if (0 != status) {
			break;
		}
		init_crypto();
		INIT_INPUT_BUFFER;
		if (INFO >= config->verbosity_level) { // Record pertinent ydb_* env vars if -vv or higher verbosity is specified
			char *ptr;
			char *envvar_array[]
			    = {"ydb_boolean",  "ydb_chset",	  "ydb_crypt_config", "ydb_dir",	   "ydb_dist",
			       "ydb_gbldir",   "ydb_lct_stdnull", "ydb_rel",	      "ydb_repl_instance", "ydb_repl_instname",
			       "ydb_routines", "ydb_xc_ydbposix", "ydb_side_effects"};
			unsigned int i;

			INFO(INFO_RECORDING_ENV_VARS, "");
			for (i = 0; i < (sizeof(envvar_array) / sizeof(envvar_array[0])); i++) {
				ptr = getenv(envvar_array[i]);
				if (NULL == ptr)
					ptr = "";
				INFO(INFO_ENV_VAR, envvar_array[i], ptr);
			}
		}
		/* Determine full path of "ydbocto.xc" to set as the external table path for date/time c functions */
		if (!DISABLE_INSTALL) {
			/* Octo was installed under $ydb_dist (using "cmake -D DISABLE_INSTALL=OFF"). So pick that path.
			 * NOTE: this uses a hard-coded path under $ydb_dist.
			 */
			ydb_dist = getenv("ydb_dist");
			if (NULL != ydb_dist) {
				status = snprintf(xc_path, sizeof(xc_path), "%s/plugin/octo/ydbocto.xc", ydb_dist);
				if ((int)sizeof(xc_path) <= status) {
					ERROR(ERR_BUFFER_TOO_SMALL,
					      "Octo external call table path : $ydb_dist/plugin/octo/ydbocto.xc");
					status = 1;
					break;
				}
			} else {
				ERROR(ERR_FAILED_TO_RETRIEVE_ENVIRONMENT_VARIABLE, "ydb_dist");
				status = 1;
				break;
			}
			/* DELETE ^%ydboctoocto("xc_path") */
			ydb_buffer_t octo_global, sub;
			YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
			YDB_STRING_TO_BUFFER(OCTOLIT_XC_PATH, &sub);
			status = ydb_delete_s(&octo_global, 1, &sub, YDB_DEL_NODE);
			if (YDB_OK != status) {
				YDB_ERROR_CHECK(status);
				status = 1;
				break;
			}
		} else {
			/* Octo was built but not installed. So derive the path of ydbocto.xc from the path of the
			 * octo/rocto binary that is currently running.
			 */
			exe_path_len = readlink("/proc/self/exe", exe_path, OCTO_PATH_MAX);
			if ((-1 != exe_path_len) && (OCTO_PATH_MAX > exe_path_len)) {
				exe_path[exe_path_len] = '\0'; // readlink() doesn't add a null terminator per man page
				src_path = dirname(exe_path);
				if (NULL != src_path) {
					status = snprintf(xc_path, sizeof(xc_path), "%s/ydbocto.xc", src_path);
					if ((int)sizeof(xc_path) <= status) {
						ERROR(ERR_BUFFER_TOO_SMALL, "ydbocto.xc external call table file path");
						status = 1;
						break;
					}
				} else {
					ERROR(ERR_LIBCALL_WITH_ARG, "dirname()", exe_path);
					status = 1;
					break;
				}
			} else {
				ERROR(ERR_LIBCALL_WITH_ARG, "readlink()", "/proc/self/exe");
				status = 1;
				break;
			}
			/* Set gvn with xc_path so that aux/_ydboctoplanhelpers.m -> transformation functions can set ydb_xc_octo
			 * when DISABLE_INSTALL is set to OFF
			 */
			// $get(gvn)
			ydb_buffer_t octo_global, sub, path;
			char	     path_buff[OCTO_PATH_MAX];
			path.buf_addr = &path_buff[0];
			path.len_alloc = sizeof(path_buff);
			YDB_STRING_TO_BUFFER(config->global_names.octo, &octo_global);
			YDB_STRING_TO_BUFFER(OCTOLIT_XC_PATH, &sub);
			status = ydb_get_s(&octo_global, 1, &sub, &path);
			if ((YDB_OK == status) || (YDB_ERR_GVUNDEF == status)) {
				boolean_t set_path = FALSE;
				if (YDB_OK == status) {
					path.buf_addr[path.len_used] = '\0';
					// If set, check that ydbocto.xc exists in the path given by gvn value if not set it to
					// xc_path
					FILE *fp;
					fp = fopen(path_buff, "r");
					if (NULL == fp) {
						set_path = TRUE;
					}
				} else {
					assert(YDB_ERR_GVUNDEF == status);
					set_path = TRUE;
				}
				if (set_path) {
					YDB_STRING_TO_BUFFER(xc_path, &path)
					status = ydb_set_s(&octo_global, 1, &sub, &path);
					if (YDB_OK != status) {
						// ERROR
						YDB_ERROR_CHECK(status);
						status = 1;
						break;
					}
				}
			} else {
				// ERROR
				YDB_ERROR_CHECK(status);
				status = 1;
				break;
			}
		}
		setenv("ydb_xc_octo", xc_path, TRUE); // Overwrite
		/* Determine full path of "ydbocto.ci" to set as the call-in table path */
		if (!DISABLE_INSTALL) {
			/* Octo was installed under $ydb_dist (using "cmake -D DISABLE_INSTALL=OFF"). So pick that path.
			 * NOTE: this uses a hard-coded path under $ydb_dist. Not the currently active $ydb_ci.
			 */
			ydb_dist = getenv("ydb_dist");
			if (NULL != ydb_dist) {
				status = snprintf(ci_path, sizeof(ci_path), "%s/plugin/octo/ydbocto.ci", ydb_dist);
				if ((int)sizeof(ci_path) <= status) {
					ERROR(ERR_BUFFER_TOO_SMALL, "Octo call-in table path : $ydb_dist/plugin/octo/ydbocto.ci");
					status = 1;
					break;
				}
			} else {
				ERROR(ERR_FAILED_TO_RETRIEVE_ENVIRONMENT_VARIABLE, "ydb_dist");
				status = 1;
				break;
			}
		} else {
			/* Octo was built but not installed. So derive the path of ydbocto.ci from the path of the
			 * octo/rocto binary that is currently running.
			 */
			exe_path_len = readlink("/proc/self/exe", exe_path, OCTO_PATH_MAX);
			if ((-1 != exe_path_len) && (OCTO_PATH_MAX > exe_path_len)) {
				exe_path[exe_path_len] = '\0'; // readlink() doesn't add a null terminator per man page
				src_path = dirname(exe_path);
				if (NULL != src_path) {
					status = snprintf(ci_path, sizeof(ci_path), "%s/ydbocto.ci", src_path);
					if ((int)sizeof(ci_path) <= status) {
						ERROR(ERR_BUFFER_TOO_SMALL, "ydbocto.ci call-in table file path");
						status = 1;
						break;
					}
				} else {
					ERROR(ERR_LIBCALL_WITH_ARG, "dirname()", exe_path);
					status = 1;
					break;
				}
			} else {
				ERROR(ERR_LIBCALL_WITH_ARG, "readlink()", "/proc/self/exe");
				status = 1;
				break;
			}
		}
		status = ydb_ci_tab_open(ci_path, &ci_tab_handle_new);
		if (YDB_OK == status) {
			status = ydb_ci_tab_switch(ci_tab_handle_new, &ci_tab_handle_old);
		}
		if (YDB_OK == status) {
			status = ydb_ci("_ydboctoInit", &ci_return, (ydb_int_t)config->verbosity_level);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			status = 1;
			break;
		} else if (0 != ci_return) {
			ERROR(ERR_NULL_SUBS_DISABLED, "");
			status = 1;
			break;
		}
		/* Set "ydb_release_number" global variable from the $ZYRELEASE ISV.
		 * Note: "argc" can be 0 in case of cmocka unit tests. Do not attempt "ydb_get_s()" in that case
		 *	as a few cmocka tests fail in that case.
		 */
		if (argc) {
			/* Store YottaDB release number as an integer (e.g. 130 in case of r1.30 etc.) */
			YDB_LITERAL_TO_BUFFER("$ZYRELEASE", &zyrelease);
			OCTO_SET_NULL_TERMINATED_BUFFER(zyrel_value, zyrel_value_buff);
			ydb_get_s(&zyrelease, 0, NULL, &zyrel_value);
			zyrel_value.buf_addr[zyrel_value.len_used] = '\0';
			/* $ZYRELEASE is of the form "YottaDB r1.30 Linux x86_64". Extract the 130 from it */
			strtok(zyrel_value.buf_addr, " "); /* ch now points to "YottaDB" */
			ch = strtok(NULL, " ");		   /* ch now points to "r1.30" */
			ch++;				   /* Skip past "r" in "r1.30" */
			strtok(ch, ".");
			ch2 = strtok(NULL, "."); /* ch now points to major version "1", ch2 now points to minor version "30" */
			ydb_release_number = atoi(ch) * 100;
			/* Note: ch2 can be NULL in case release is numbered say r999 (i.e. only major version, no minor version) */
			if (NULL != ch2) {
				ydb_release_number += atoi(ch2);
			}
		}
		/* Set global variables to non-default values if necessary (default value is 0) */
		hash_canonical_query_cycle = (uint64_t)INT32_MAX;
		/* This global variable is incremented before every outermost call to "hash_canonical_query".
		 * It is initialized to INT32_MAX instead of 0 (default for most globals) since the field that this is
		 * checked for equality (in "hash_canonical_query()") is "stmt->hash_canonical_query_cycle" and the latter
		 * can take on an overloaded meaning of a 4-byte signed integer "max_unique_id" (see comments in
		 * "SqlStatement" in "octo_types.h" for more details). Initializing this to INT32_MAX avoids this field
		 * from incorrectly/coincidentally being set to the exact same value as "max_unique_id" (YDBOcto#790).
		 */
		config->config_file = config_file;
		/* "argc" can be 0 in case of cmocka unit tests. Do not attempt auto upgrade/load in that case. */
		if (argc) {
			/* If running rocto, the following auto upgrade/load logic will execute before any user has logged in,
			 * leading to failed updates due to a lack of adequate permissions. This results from 0-initialization of
			 * rocto_session in parse_startup_flags.c, thus setting the default permissions level to
			 * UserPermissions_ReadOnly. So, temporarily set rocto_session.permissions to
			 * UserPermissions_RWAllowSchemaChanges to allow the following code to execute.
			 */
			if (config->is_rocto) {
				assert(UserPermissions_ReadOnly == rocto_session.permissions);
				rocto_session.permissions = UserPermissions_RWAllowSchemaChanges;
			}
			/* Quit auto-upgrade if DB has higher binary definition format as artifacts might exist which will
			 * be incompatible with the current binary definition format.
			 */
			status = is_auto_upgrade_valid();
			if (YDB_OK != status) {
				/* Error message would have been printed already inside the above function call */
				break;
			}
			/* Check if plan-definitions (of tables or functions) need to be auto upgraded (due to format changes).  *
			 * If so discard them (they will be regenerated as needed).  */
			status = auto_upgrade_plan_definition_if_needed();
			if (YDB_OK != status) {
				/* Error message would have been printed already inside the above function call */
				break;
			}
			/* Check if octo-seed needs to be reloaded (due to a newer Octo build). If so do that. */
			status = auto_load_octo_seed_if_needed();
			if (YDB_OK != status) {
				/* Error message would have been printed already inside the above function call */
				break;
			}
			/* Check if binary-definitions (of tables or functions) need to be auto upgraded. If so upgrade
			 * them. */
			status = auto_upgrade_binary_definition_if_needed();
			if (YDB_OK != status) {
				/* Error message would have been printed already inside the above function call */
				break;
			}
			if (config->is_rocto) {
				rocto_session.permissions = UserPermissions_ReadOnly;
			}
		}
		// Now that configuration is complete, load `pg_settings` defaults into LVNs for process-local access
		status = load_pg_defaults();
		if (YDB_OK != status) {
			break;
		}
		return 0;
	}
	// In case there was an error inside auto upgrade/load, rocto user permissions can be returned to the read-only default
	if (config->is_rocto) {
		rocto_session.permissions = UserPermissions_ReadOnly;
	}
	// No call to load_pg_settings is needed here since we only get here if there was a configuration error, so assert this.
	assert(0 != status);
	CLEANUP_CONFIG(config_file);
	return status;
}
