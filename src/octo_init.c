/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

#include <readline.h>

#include "octo.h"
#include "octo_types.h"

// Read binary file with default config settings
#include "default_octo_conf.h"

#define	OCTO_CONF_FILE_NAME	"octo.conf"

#define MAX_CONFIG_FILES	3

#define SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(NAME, CARET)							\
{															\
	int	length;													\
															\
	length = snprintf(config->global_names.NAME, sizeof(config->global_names.NAME), "%s%%ydbocto%s", CARET, #NAME);	\
	if ((0 > length) || ((int)sizeof(config->global_names.NAME) <= length)) {					\
		ERROR(ERR_MIDENT_LENGTH, length, YDB_MAX_IDENT);							\
		return 1;												\
	}														\
	assert('\0' == config->global_names.NAME[length]);								\
}

#define MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR(FORMAT, ENV_VAR, CONFIG_FILE, CONFIG_FILE_LIST, FILENAME)				\
{																\
	int	tmp_path_len, status;												\
																\
	tmp_path_len = snprintf(FILENAME, OCTO_PATH_MAX, FORMAT, ENV_VAR,							\
			OCTO_CONF_FILE_NAME);											\
	/* Ignore this configuration file if snprintf output was truncated. */							\
	if (OCTO_PATH_MAX > tmp_path_len) {											\
		assert('\0' == FILENAME[tmp_path_len]);										\
		status = merge_config_file(FILENAME, &CONFIG_FILE, CONFIG_IMPLICIT);							\
		if (0 == status) {												\
			CONFIG_FILE_LIST.filenames[CONFIG_FILE_LIST.num_files] = FILENAME;					\
			CONFIG_FILE_LIST.num_files++;										\
			assert(MAX_CONFIG_FILES >= CONFIG_FILE_LIST.num_files);							\
		} else if (1 == status) {											\
			return 1;												\
		} else {													\
			/* The file wasn't found or was inaccessible, so don't add file to the list of loaded files. */		\
		}														\
	} else {														\
		return 1;													\
	}															\
}

/* Check for libconfig errors reported after attempting to read a configuration setting. In case of an error, report it and return.
 * Note that this check is only done after another libconfig call failed, and is used to differentiate between the various failure
 * cases of the preceding call. Particularly, this allows us to distinguish syntax errors in a setting from that setting simply
 * being omitted (the CONFIG_ERR_NONE case).
 */
#define CONFIG_ERROR_CHECK(CONFIG_FILE, SETTING_NAME, RESULT)									\
{																\
	int	error_type;													\
																\
	error_type = config_error_type(CONFIG_FILE);										\
	switch (error_type) {													\
	case CONFIG_ERR_FILE_IO:												\
		ERROR(ERR_CONFIG_IO_FAILURE, SETTING_NAME, config_file_name);							\
		return 1;													\
		break;														\
	case CONFIG_ERR_PARSE:													\
		ERROR(ERR_BAD_CONFIG, config_file_name, SETTING_NAME);								\
		return 1;													\
		break;														\
	default:														\
		assert(CONFIG_ERR_NONE == error_type);										\
		break;														\
	}															\
}

/* Contains a list of names for configuration files and the total number of such files.
 * Used for outputting a list of all configuration files loaded.
 */
typedef struct config_file_list {
	char *filenames[MAX_CONFIG_FILES];
	int num_files;
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

int parse_config_file_settings(const char *config_file_name, config_t *config_file);
void merge_config_file_helper(config_setting_t *a, config_setting_t *b);

/* Returns 0 for success, 1 on error, and 2 for file not found/inaccessible (not technically an error, but must be distinguished for
 * so the caller only issues a config loaded message when the file is actually used)
 * The is_default flag is used to indicate that the default configuration should be merged in. This is needed since the default
 * config is read from a string, not a file, so this case must be treated separately.
 */
int merge_config_file(const char *path, config_t **config_file, enum config_kind kind) {
	config_setting_t	*a_root, *b_root;
	config_t		*new_config_file;
	const char		*error_message, *error_file;
	char			*default_octo_conf;
	int			error_line, status;

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
		default_octo_conf = (char *)malloc(octo_conf_default_len + 1);
		memcpy(default_octo_conf, octo_conf_default, octo_conf_default_len);
		default_octo_conf[octo_conf_default_len] = '\0';
		status = config_read_string(new_config_file, default_octo_conf);
		free(default_octo_conf);
		if (CONFIG_FALSE == status) {
			error_message = config_error_text(new_config_file);
			error_file = "default";
			error_line = config_error_line(new_config_file);
			ERROR(ERR_PARSING_CONFIG, error_file, error_line, error_message);
			CLEANUP_CONFIG(new_config_file);
			return 1;
		}
		path = "default";	// Use literal in place of file path when issuing errors in parse_config_file_settings
	}
	a_root = config_root_setting(*config_file);
	b_root = config_root_setting(new_config_file);
	merge_config_file_helper(b_root, a_root);
	CLEANUP_CONFIG(*config_file);
	*config_file = new_config_file;
	status = parse_config_file_settings(path, *config_file);
	return status;
}

// Merges b into a, updating a with any values from b
void merge_config_file_helper(config_setting_t *a, config_setting_t *b) {
	config_setting_t	*t_setting, *b_setting;
	char			*setting_name;
	int			setting_type, b_index;

	b_index = 0;
	while (TRUE) {
		b_setting = config_setting_get_elem(b, b_index);
		if (NULL == b_setting)
			break;
		setting_name = config_setting_name(b_setting);
		setting_type = config_setting_type(b_setting);
		if (config_setting_is_group(b_setting) || config_setting_is_array(b_setting)
				|| config_setting_is_list(b_setting)) {
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
	config_setting_t	*ydb_settings, *cur_ydb_setting;
	ydb_buffer_t		zroutines_buffer, dollar_zroutines_buffer;
	unsigned int		offset, zroutines_from_file_len, zroutines_len = ZRO_INIT_ALLOC;
	const char		*item_name, *item_value, *verbosity;
	char			*zroutines_buf_start, *zroutines_from_file;
	int			status, done, i, verbosity_int;

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
			ERROR(ERR_BAD_CONFIG, config_file_name, "verbosity");
			return 1;
		}
	} else if (CONFIG_FALSE == config_lookup_int(config_file, "verbosity", &verbosity_int)) {
		CONFIG_ERROR_CHECK(config_file, "verbosity", status);
		verbosity_int = ERROR;		// Set to the default if verbosity was not set, i.e. the error check succeeds
	}
	config->verbosity_level = verbosity_int;
	if (CONFIG_FALSE == config_lookup_string(config_file, "rocto.address", &config->rocto_config.address)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.address", status);
	}
	if (CONFIG_FALSE == config_lookup_int(config_file, "rocto.port", &config->rocto_config.port)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.port", status);
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.use_dns", &config->rocto_config.use_dns)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.use_dns", status);
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.tcp_delay", &config->rocto_config.tcp_delay)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.tcp_delay", status);
	}
#	if YDB_TLS_AVAILABLE
	if (CONFIG_FALSE == config_lookup_string(config_file, "tls.OCTOSERVER.cert", &config->rocto_config.ssl_cert_file)) {
		CONFIG_ERROR_CHECK(config_file, "tls.OCTOSERVER.cert", status);
	}
	if (CONFIG_FALSE == config_lookup_string(config_file, "tls.OCTOSERVER.key", &config->rocto_config.ssl_key_file)) {
		CONFIG_ERROR_CHECK(config_file, "tls.OCTOSERVER.key", status);
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.ssl_on", status);
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.ssl_required", &config->rocto_config.ssl_required)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.ssl_required", status);
	}
	if (!config->rocto_config.ssl_on && config->rocto_config.ssl_required) {
		ERROR(ERR_BAD_CONFIG, config_file_name, "rocto.ssl_required set, but rocto.ssl_on is disabled");
		return 1;
	}

#	else
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.ssl_on", status);
	}
	if (config->rocto_config.ssl_on) {
		ERROR(ERR_BAD_CONFIG, config_file_name, "rocto.ssl_on set, but YottaDB TLS plugin not installed");
		return 1;
	}
	if (CONFIG_FALSE == config_lookup_bool(config_file, "rocto.ssl_required", &config->rocto_config.ssl_required)) {
		CONFIG_ERROR_CHECK(config_file, "rocto.ssl_required", status);
	}
	if (config->rocto_config.ssl_required) {
		ERROR(ERR_BAD_CONFIG, config_file_name, "rocto.ssl_required set, but YottaDB TLS plugin not installed");
		return 1;
	}
#	endif
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

	// $ZROUTINES has already been set, just return now
	if (NULL != config->tmp_dir) {
		return 0;
	}
	YDB_MALLOC_BUFFER(&zroutines_buffer, ZRO_INIT_ALLOC);
	YDB_LITERAL_TO_BUFFER("$zroutines", &dollar_zroutines_buffer);
	// Prepend zroutines path from configuration file to the start of $zroutines, if specified.
	if (CONFIG_TRUE == config_lookup_string(config_file, "octo_zroutines", (const char**) &zroutines_from_file)) {
		zroutines_from_file_len = strlen(zroutines_from_file);
		YDB_COPY_STRING_TO_BUFFER(zroutines_from_file, &zroutines_buffer, done);
		// Double size if ZRO_INIT_ALLOC is too small to avoid potential resize later.
		if (!done) {
			YDB_FREE_BUFFER(&zroutines_buffer);
			zroutines_len = (zroutines_from_file_len + 1) * 2;	// ' ' + '\0'
			YDB_MALLOC_BUFFER(&zroutines_buffer, zroutines_len);
			YDB_COPY_STRING_TO_BUFFER(zroutines_from_file, &zroutines_buffer, done);
			if (!done) {
				ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
				YDB_FREE_BUFFER(&zroutines_buffer);
				return 1;
			}
		}
		do {
			// Shift start of buffer to after the zroutines path pulled from the configuration file
			zroutines_buf_start = zroutines_buffer.buf_addr;
			zroutines_buffer.buf_addr[zroutines_from_file_len] = ' ';
			zroutines_buffer.buf_addr = zroutines_buf_start + zroutines_from_file_len + 1;
			zroutines_buffer.len_alloc = zroutines_len - zroutines_from_file_len - 2;	// ' ' + '\0'
			zroutines_buffer.len_used = 0;
			// Retrieve value of $zroutines
			status = ydb_get_s(&dollar_zroutines_buffer, 0, NULL, &zroutines_buffer);
			if (YDB_ERR_INVSTRLEN == status) {
				/* If the buffer isn't large enough allocate more and call ydb_get_s() again
				 * +2 for null terminator and space
				 */
				zroutines_len = zroutines_buffer.len_used + zroutines_from_file_len + 2;
				zroutines_buffer.buf_addr = zroutines_buf_start;
				YDB_FREE_BUFFER(&zroutines_buffer);
				YDB_MALLOC_BUFFER(&zroutines_buffer, zroutines_len);
				YDB_COPY_STRING_TO_BUFFER(zroutines_from_file, &zroutines_buffer, done);
				if (!done) {
					ERROR(ERR_YOTTADB, "YDB_COPY_STRING_TO_BUFFER failed");
					zroutines_buffer.buf_addr = zroutines_buf_start;
					YDB_FREE_BUFFER(&zroutines_buffer);
					return 1;
				}
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
		zroutines_buffer.len_used += zroutines_from_file_len + 1;	// Space character ' '
		zroutines_buffer.len_alloc = zroutines_len;

		status = ydb_set_s(&dollar_zroutines_buffer, 0, NULL, &zroutines_buffer);
		if (YDB_ERR_ZROSYNTAX == status || YDB_ERR_DLLNOOPEN == status){
			ERROR(ERR_BAD_CONFIG, config_file_name, "octo_zroutines");
			YDB_FREE_BUFFER(&zroutines_buffer);
			return 1;
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status){
			YDB_FREE_BUFFER(&zroutines_buffer);
			return 1;
		}
		assert(zroutines_buffer.len_alloc >= zroutines_buffer.len_used + 1);
		zroutines_buffer.buf_addr[zroutines_buffer.len_used] = '\0';	// Null terminate env var
		setenv("ydb_routines", zroutines_buffer.buf_addr, TRUE);
	} else {
		// Otherwise just get $zroutines
		status = ydb_get_s(&dollar_zroutines_buffer, 0, NULL, &zroutines_buffer);
		// If ZRO_INIT_ALLOC isn't large enough allocate more and call ydb_get_s() again
		if (YDB_ERR_INVSTRLEN == status) {
			zroutines_len = zroutines_buffer.len_used + 2;		// Null terminator plus padding space
			YDB_FREE_BUFFER(&zroutines_buffer);
			YDB_MALLOC_BUFFER(&zroutines_buffer, zroutines_len);
			status = ydb_get_s(&dollar_zroutines_buffer, 0, NULL, &zroutines_buffer);
		}
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status){
			YDB_FREE_BUFFER(&zroutines_buffer);
			return 1;
		}
	}
	zroutines_buf_start = zroutines_buffer.buf_addr;

	// Trim leading white space
	offset = 0;
	status = TRUE;
	while (status && (offset < zroutines_buffer.len_used)){
		switch (zroutines_buffer.buf_addr[offset]) {
		case ' ' :
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

	// Extract the leading path and store in config->tmp_dir
	offset = 0;
	struct stat statbuf;
	while ('\0' != zroutines_buffer.buf_addr[offset]){
		switch(zroutines_buffer.buf_addr[offset]){
		// White space characters and right parenthesis are delimiters
		case ' ' :
		case ')' :
			// Terminate everything read up to this point, if last character is a '*' remove it
			if (1 < offset){
				if ('*' == zroutines_buffer.buf_addr[offset-1]){
					offset--;
				}
			}
			// The current path string is the empty string, skip and continue.
			if (0 == offset) {
				/* If the next character (offset+1) is the end of the buffer, the loop will break on '\0'
				 * and an error will be issued below for an invalid zroutines source path (empty string).
				 */
				zroutines_buffer.buf_addr += (offset+1);
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
				zroutines_buffer.buf_addr += (offset+1);
				offset = 0;
				break;
			}
			// +1 for null terminator
			config->tmp_dir = malloc(offset + 1);
			// cast to char* to avoid strcpy warning this is safe to do here
			strncpy((char*) config->tmp_dir, zroutines_buffer.buf_addr, offset + 1);
			break;
		case '(':
			// If a '(' is found shift start of string
			zroutines_buffer.buf_addr += (offset+1);
			offset = 0;
			break;
		default:
			offset++;
			break;
		}
	}
	zroutines_buffer.buf_addr = zroutines_buf_start;
	YDB_FREE_BUFFER(&zroutines_buffer);
	if (!config->tmp_dir){
		ERROR(ERR_BAD_ZROUTINES, NULL);
		return 1;
	}

	return 0;
}

int populate_global_names() {
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(schema, "^");				/* ^%ydboctoschema */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(session, "");				/*  %ydboctosession */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(cursor, "");				/*  %ydboctocursor */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(octo, "^");				/* ^%ydboctoocto */
	config->global_names.raw_octo = &config->global_names.octo[1];				/*  %ydboctoocto */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(xref, "^");				/* ^%ydboctoxref */
	config->global_names.raw_xref = &config->global_names.xref[1];				/*  %ydboctoxref */
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(loadedschemas, "");			/*  %ydboctoloadedschemas */
	return 0;
}

void init_crypto() {
	/* Load the human readable error strings for libcrypto */
	ERR_load_crypto_strings();

	/* Load all digest and cipher algorithms */
	OpenSSL_add_all_algorithms();

	/* Load default config file, and other important initialisation */
	CONF_modules_load_file(NULL, NULL, 0);
}

int octo_init(int argc, char **argv) {
	OctoConfig		temp_config;
	const char		*src_path;
	ydb_long_t		ci_return;
	uintptr_t		ci_tab_handle_new, ci_tab_handle_old;
	boolean_t		verbosity_set;
	config_t		*config_file;
	ssize_t			exe_path_len;
	char			ci_path[OCTO_PATH_MAX], exe_path[OCTO_PATH_MAX], cwd[OCTO_PATH_MAX];
	char			cwd_file_name[OCTO_PATH_MAX], homedir_file_name[OCTO_PATH_MAX], plugin_file_name[OCTO_PATH_MAX];
	char			zstatus_message[YDB_MAX_ERRORMSG];
	char			*homedir, *ydb_dist, *most_recent_filename, *config_file_name = NULL;
	int			status, i;
	DIR			*dir;
	ConfigFileList		config_file_list;

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
	/* Set initial verbosity to the default to rollback the verbosity_unset case in parse_startup_flags and
	 * allow errors during config merging to be reported
	 */
	config->verbosity_level = ERROR;

	// Search for config file octo.conf (OCTO_CONF_FILE_NAME) in directories ".", "~", and "$ydb_dist/plugin/octo" in that order
	config_init(config_file);

	// This loop is only ever executed once
	for (;;) {
		// This should always be 1
		setenv("ydb_lvnullsubs", "1", 1);
		status = ydb_init();
		if (YDB_OK != status) {
			ydb_zstatus(zstatus_message, sizeof(zstatus_message));
			ERROR(ERR_YOTTADB, zstatus_message);
			status = 1;
			break;
		}


		// Load config file
		config_file_list.num_files = 0;
		ydb_dist = getenv("ydb_dist");
		if (NULL == config_file_name) {
			if (NULL == getcwd(cwd, sizeof(cwd))) {
				ERROR(ERR_SYSCALL, "getcwd", errno, strerror(errno));
				status = 1;
				break;
			}
			MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR("%s/%s", cwd, config_file, config_file_list, cwd_file_name);
			homedir = getenv("HOME");
			if (NULL != homedir) {
				MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR("%s/%s", homedir, config_file, config_file_list, homedir_file_name);
			}
			if (NULL != ydb_dist) {
				MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR("%s/plugin/octo/%s", ydb_dist, config_file, config_file_list, plugin_file_name);
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
			most_recent_filename = config_file_list.filenames[config_file_list.num_files-1];
		}
		status = parse_config_file_settings(most_recent_filename, config_file);
		if (status) {
			break;
		}

		// Apply startup flags from initial parse to overwrite values from config files
		if (verbosity_set) {	// Only overwrite if initialized
			config->verbosity_level = temp_config.verbosity_level;
		}
		if (-1 != temp_config.rocto_config.port) {	// Only overwrite if initialized
			config->rocto_config.port = temp_config.rocto_config.port;
		}
		// Issue INFO messages for loaded configuration files now that verbosity level is finalized
		for (i = 0; i < config_file_list.num_files; i++) {
			assert(NULL != config_file_list.filenames[i]);
			INFO(INFO_LOADED_CONFIG, config_file_list.filenames[i]);
		}

		// Verify that the directory exists, or issue an error
		dir = opendir(config->tmp_dir);
		if (NULL == dir) {
			ERROR(ERR_SYSCALL_WITH_ARG, "opendir (config.tmp_dir)", errno, strerror(errno), config->tmp_dir);
			status = 1;
			break;
		}
		free(dir);

		config->page_size = sysconf(_SC_PAGESIZE);
		status = populate_global_names();
		if (0 != status) {
			break;
		}
		init_crypto();

		definedTables = NULL;
		// Leave space for null terminator
		// `read` takes the number of bytes to read _excluding_ the null terminator,
		// and we pass cur_input_max directly into read in `readline_get_more`.
		cur_input_max = MAX_STR_CONST - 1;
		input_buffer_combined = malloc(MAX_STR_CONST);
		memset(input_buffer_combined, 0, MAX_STR_CONST);
		old_input_index = 0;
		cur_input_index = 0;
		cur_input_more = &no_more;
		eof_hit = EOF_NONE;

		if (INFO >= config->verbosity_level) {	// Record pertinent ydb_* env vars if -vv or higher verbosity is specified
			char		*ptr;
			char		*envvar_array[] = { "ydb_dist", "ydb_gbldir", "ydb_routines", "ydb_xc_ydbposix" };
			unsigned int	i;

			INFO(CUSTOM_ERROR, "# Recording pertinent ydb_* env var values at process startup");
			for (i = 0; i < (sizeof(envvar_array) / sizeof(envvar_array[0])); i++)
			{
				ptr = getenv(envvar_array[i]);
				if (NULL == ptr)
					ptr = "";
				INFO(CUSTOM_ERROR, "# %s=\"%s\"", envvar_array[i], ptr);
			}
		}
		// NOTE: this uses hard-coded paths, not $ydb_ci
		if (!DISABLE_INSTALL) {
			if (NULL != ydb_dist)  {
				status = snprintf(ci_path, sizeof(ci_path), "%s/plugin/octo/ydbocto.ci", ydb_dist);
				if ((int)sizeof(ci_path) <= status) {
					ERROR(ERR_BUFFER_TOO_SMALL, "Octo call-in table path");
					status = 1;
					break;
				}
			} else {
				ERROR(ERR_FAILED_TO_RETRIEVE_ENVIRONMENT_VARIABLE, "ydb_dist");
				status = 1;
				break;
			}
		} else {
			exe_path_len = readlink("/proc/self/exe", exe_path, OCTO_PATH_MAX);
			if ((-1 != exe_path_len) && (OCTO_PATH_MAX > exe_path_len)) {
				exe_path[exe_path_len] = '\0';		// readlink() doesn't add a null terminator per man page
				src_path = dirname(exe_path);
				if (NULL != src_path) {
					status = snprintf(ci_path, sizeof(ci_path), "%s/ydbocto.ci", src_path);
					if ((int)sizeof(ci_path) <= status) {
						ERROR(ERR_BUFFER_TOO_SMALL, "Octo call-in table path");
						status = 1;
						break;
					}
				} else {
					ERROR(ERR_LIBCALL_WITH_ARG, "dirname", exe_path);
					status = 1;
					break;
				}
			} else {
				ERROR(ERR_LIBCALL_WITH_ARG, "readlink", "/proc/self/exe");
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
		/* readlines setup */
		rl_bind_key ('\t', rl_insert); // display the tab_completion of '\t' and just insert it as a character
		config->config_file = config_file;
		return 0;
	}
	CLEANUP_CONFIG(config_file);
	return status;
}
