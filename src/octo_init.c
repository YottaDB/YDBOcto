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

#define SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(NAME, CARET)								\
{																\
	int	length;														\
																\
	length = snprintf(config->global_names.NAME, sizeof(config->global_names.NAME), "%s%s%s", CARET, global_prefix, #NAME);	\
	if ((0 > length) || ((int)sizeof(config->global_names.NAME) <= length)) {						\
		ERROR(ERR_MIDENT_LENGTH, length, YDB_MAX_IDENT);								\
		return 1;													\
	}															\
}

#define MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR(FORMAT, ENV_VAR, CONFIG_FILE)							\
{																\
	int	tmp_path_len, status;												\
	char	config_path[OCTO_PATH_MAX];											\
																\
	tmp_path_len = snprintf(config_path, sizeof(config_path), FORMAT, ENV_VAR, OCTO_CONF_FILE_NAME);			\
	/* Ignore this configuration file if snprintf output was truncated. */							\
	if (tmp_path_len < (int)sizeof(config_path)) {										\
		assert('\0' == config_path[tmp_path_len]);									\
		status = merge_config_file(config_path, CONFIG_FILE);								\
		if (0 != status) {												\
			return 1;												\
		}														\
	}															\
}

void merge_config_file_helper(config_setting_t *a, config_setting_t *b);

int merge_config_file(const char *path, config_t *config_file) {
	config_setting_t	*a_root, *b_root;
	config_t		new_config_file;
	const char		*error_message, *error_file;
	int			error_line;

	if (-1 == access(path, F_OK)) {
		// File not found or no access; skip it
		return 0;
	}
	config_init(&new_config_file);
	if (CONFIG_FALSE == config_read_file(&new_config_file, path)) {
		error_message = config_error_text(config_file);
		error_file = config_error_file(config_file);
		error_line = config_error_line(config_file);
		ERROR(ERR_PARSING_CONFIG, error_file, error_line, error_message);
		return 1;
	}
	INFO(ERR_LOADING_CONFIG, path);
	a_root = config_root_setting(config_file);
	b_root = config_root_setting(&new_config_file);
	merge_config_file_helper(a_root, b_root);
	config_destroy(&new_config_file);
	return 0;
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

int populate_global_names() {
	char	*global_prefix = "%ydbocto";

	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(schema, "^");
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(octo, "^");
	config->global_names.raw_octo = &config->global_names.octo[1];
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(xref, "^");
	config->global_names.raw_xref = &config->global_names.xref[1];
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(session, "");
	SET_CONFIG_VARIABLE_NAME_AND_RETURN_ON_ERROR(cursor, "");
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
	int			status, done, i;
	unsigned int		offset, zroutines_from_file_len, zroutines_len = ZRO_INIT_ALLOC;
	config_t		*config_file;
	config_setting_t	*ydb_settings, *cur_ydb_setting;
	const char		*item_name, *item_value, *src_path;
	char			*default_octo_conf;
	char			ci_path[OCTO_PATH_MAX], exe_path[OCTO_PATH_MAX];
	char			*homedir, *ydb_dist, *zroutines_buf_start, *zroutines_from_file;
	uintptr_t		ci_tab_handle_new, ci_tab_handle_old;
	ydb_long_t		ci_return;
	ydb_buffer_t		zroutines_buffer, dollar_zroutines_buffer;
	DIR			*dir;
	ssize_t			exe_path_len;
	const char		*verbosity, *error_message, *error_file;
	int			verbosity_int, error_line;

	config = (OctoConfig *)malloc(sizeof(OctoConfig));
	memset(config, 0, sizeof(OctoConfig));
	config_file = &config->config_file;

	// If OCTO_SETTINGS env is set, we previously parsed the config and stashed it
	//   in environment variables; load from there

	// Parse the startup flags; we will have to do this again after we read the config
	status = parse_startup_flags(argc, argv);
	if (status)
		return status;

	// Search for config file octo.conf (OCTO_CONF_FILE_NAME) in directories "$ydb_dist/plugin/etc", "~" and "." in that order
	config_init(config_file);

	// This should always be 1
	setenv("ydb_lvnullsubs", "1", 1);
	ydb_init();

	default_octo_conf = (char *)malloc(octo_conf_default_len + 1);
	memcpy(default_octo_conf, octo_conf_default, octo_conf_default_len);
	default_octo_conf[octo_conf_default_len] = '\0';

	status = config_read_string(config_file, default_octo_conf);
	if (CONFIG_FALSE == status) {
		error_message = config_error_text(config_file);
		error_file = config_error_file(config_file);
		error_line = config_error_line(config_file);
		ERROR(ERR_PARSING_CONFIG, error_file, error_line, error_message);
		free(default_octo_conf);
		return 1;
	}

	// Load config file
	ydb_dist = getenv("ydb_dist");
	if (NULL == config->config_file_name) {
		if (NULL != ydb_dist) {
			MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR("%s/plugin/etc/%s", ydb_dist, config_file);
		}
		homedir = getenv("HOME");
		if (NULL != homedir) {
			MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR("%s/%s", homedir, config_file);
		}
		MERGE_CONFIG_PATH_AND_RETURN_ON_ERROR("./%s%s", "", config_file);
	} else {
		status = merge_config_file(config->config_file_name, config_file);
		if (0 != status) {
			return 1;
		}
	}
	free(default_octo_conf);

	if (config_lookup_string(config_file, "verbosity", &verbosity) == CONFIG_TRUE) {
		if (strcmp(verbosity, "TRACE") == 0) {
			verbosity_int = TRACE;
		} else if (strcmp(verbosity, "INFO") == 0) {
			verbosity_int = INFO;
		} else if (strcmp(verbosity, "DEBUG") == 0) {
			verbosity_int = DEBUG;
		} else if (strcmp(verbosity, "WARNING") == 0) {
			verbosity_int = WARNING;
		} else if (strcmp(verbosity, "ERROR") == 0) {
			verbosity_int = ERROR;
		} else if (strcmp(verbosity, "FATAL") == 0) {
			verbosity_int = FATAL;
		} else {
			ERROR(ERR_BAD_CONFIG, "verbosity");
			return 1;
		}
	} else if (config_lookup_int(config_file, "verbosity", &verbosity_int) == CONFIG_FALSE) {
		printf("Verbosity set to %s\n", verbosity);
		ERROR(ERR_BAD_CONFIG, "verbosity");
		return 1;
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
				// If the buffer isn't large enough allocate more and call ydb_get_s() again
				// +2 for null terminator and space
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
			ERROR(ERR_BAD_CONFIG, "octo_zroutines");
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
			// Terminate everything read up to this point,
			// if last character is a '*' remove it
			if (1 < offset){
				if ('*' == zroutines_buffer.buf_addr[offset-1]){
					offset--;
				}
			}
			// The current path string is the empty string, skip and continue.
			if (0 == offset) {
				// If the next character (offset+1) is the end of the buffer, the loop will break on '\0'
				// and an error will be issued below for an invalid zroutines source path (empty string).
				zroutines_buffer.buf_addr += (offset+1);
				break;
			}
			// Check if this path is a directory and, if not,
			// move the start of the string to the next token
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

	if (config_lookup_string(config_file, "rocto.address", &config->rocto_config.address)
			== CONFIG_FALSE) {
		ERROR(ERR_BAD_CONFIG, "rocto.address");
		return 1;
	}
	if (config_lookup_int(config_file, "rocto.port", &config->rocto_config.port)
			== CONFIG_FALSE) {
		ERROR(ERR_BAD_CONFIG, "rocto.port");
		return 1;
	}
	if (config_lookup_bool(config_file, "rocto.use_dns", &config->rocto_config.use_dns)
			== CONFIG_FALSE) {
		ERROR(ERR_BAD_CONFIG, "rocto.use_dns");
		return 1;
	}
#	if YDB_TLS_AVAILABLE
	if (config_lookup_string(config_file, "tls.OCTOSERVER.cert", &config->rocto_config.ssl_cert_file)
			== CONFIG_FALSE) {
		ERROR(ERR_BAD_CONFIG, "tls.OCTOSERVER.cert");
		return 1;
	}
	if (config_lookup_string(config_file, "tls.OCTOSERVER.key", &config->rocto_config.ssl_key_file)
			== CONFIG_FALSE) {
		ERROR(ERR_BAD_CONFIG, "tls.OCTOSERVER.key");
		return 1;
	}
	if (config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)
			== CONFIG_FALSE) {
		ERROR(ERR_BAD_CONFIG, "rocto.ssl_on");
		return 1;
	}

#	else
	if (config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)
			== CONFIG_FALSE) {
		ERROR(ERR_BAD_CONFIG, "rocto.ssl_on");
		return 1;
	}
	if (config->rocto_config.ssl_on) {
		ERROR(ERR_BAD_CONFIG, "rocto.ssl_on set, but YottaDB TLS plugin not installed");
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

	config->record_error_level = verbosity_int;
	//config->dry_run = FALSE;
	err_buffer = stderr;

	// Reparse the startup flags to ensure they overwrite the config files
	status = parse_startup_flags(argc, argv);
	if (status)
		return status;

	// Verify that the directory exists, or issue an error
	dir = opendir(config->tmp_dir);
	if (NULL == dir) {
		ERROR(ERR_SYSCALL_WITH_ARG, "opendir (config.tmp_dir)", errno, strerror(errno), config->tmp_dir);
		return 1;
	}
	free(dir);

	config->page_size = sysconf(_SC_PAGESIZE);
	status = populate_global_names();
	if (0 != status) {
		return status;
	}
	init_crypto();

	definedTables = NULL;
	cur_input_max = MAX_STR_CONST;
	input_buffer_combined = malloc(MAX_STR_CONST);
	memset(input_buffer_combined, 0, MAX_STR_CONST);
	old_input_index = 0;
	cur_input_index = 0;
	cur_input_more = &no_more;
	eof_hit = 0;

	if (INFO >= config->record_error_level)
	{	// Record pertinent ydb_* env vars if -vv or higher verbosity is specified
		char		*ptr;
		char		*envvar_array[] = { "ydb_dist", "ydb_gbldir", "ydb_routines", "ydb_ci", "ydb_xc_ydbposix" };
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
	if (!DISABLE_INSTALL) {
		if (NULL != ydb_dist)  {
			status = snprintf(ci_path, sizeof(ci_path), "%s/plugin/octo/ydbocto.ci", ydb_dist);
			if ((int)sizeof(ci_path) <= status) {
				ERROR(ERR_BUFFER_TOO_SMALL, "Octo call-in table path");
				return 1;
			}
		} else {
			ERROR(ERR_FAILED_TO_RETRIEVE_ENVIRONMENT_VARIABLE, "ydb_dist");
			return 1;
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
					return 1;
				}
			} else {
				ERROR(ERR_LIBCALL_WITH_ARG, "dirname", exe_path);
				return 1;
			}
		} else {
			ERROR(ERR_LIBCALL_WITH_ARG, "readlink", "/proc/self/exe");
			return 1;
		}
	}
	ydb_ci_tab_open(ci_path, &ci_tab_handle_new);
	ydb_ci_tab_switch(ci_tab_handle_new, &ci_tab_handle_old);
	status = ydb_ci("_ydboctoInit", &ci_return);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		return 1;
	} else if (0 != ci_return) {
		ERROR(ERR_NULL_SUBS_DISABLED, "");
		return 1;
	}
	/* readlines setup */
	rl_bind_key ('\t', rl_insert); // display the tab_completion of '\t' and just insert it as a character
	return 0;
}
