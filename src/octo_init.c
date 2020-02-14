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

#include <openssl/conf.h>
#include <openssl/evp.h>
#include <openssl/err.h>

#include <readline.h>

#include "octo.h"
#include "octo_types.h"

// Read binary file with default config settings
#include "default_octo_conf.h"

#define	OCTO_CONF_FILE_NAME	"octo.conf"

void merge_config_file_helper(config_setting_t *a, config_setting_t *b);

void merge_config_file(const char *path, config_t *config_file) {
	const char *error_message, *error_file;
	int error_line;
	config_t new_config_file;
	config_setting_t *a_root, *b_root;
	if (access(path, F_OK) == -1) {
		// File not found or no access; skip it
		return;
	}
	config_init(&new_config_file);
	if (config_read_file(&new_config_file, path) == CONFIG_FALSE) {
		error_message = config_error_text(config_file);
		error_file = config_error_file(config_file);
		error_line = config_error_line(config_file);
		FATAL(ERR_PARSING_CONFIG, error_file,
				error_line,
				error_message);
	}
	INFO(ERR_LOADING_CONFIG, path);
	a_root = config_root_setting(config_file);
	b_root = config_root_setting(&new_config_file);
	merge_config_file_helper(a_root, b_root);
	config_destroy(&new_config_file);
}

/**
 * Merges b into a, updating a with any values from b
 */
void merge_config_file_helper(config_setting_t *a, config_setting_t *b) {
	config_setting_t *t_setting, *b_setting;
	char *setting_name;
	int setting_type;
	int b_index;
	b_index = 0;
	while (TRUE) {
		b_setting = config_setting_get_elem(b, b_index);
		if (NULL == b_setting)
			break;
		setting_name = config_setting_name(b_setting);
		setting_type = config_setting_type(b_setting);
		if (config_setting_is_group(b_setting) || config_setting_is_array(b_setting)
				|| config_setting_is_list(b_setting)) {
			switch(config_setting_type(a))
			{
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
			switch(config_setting_type(b_setting))
			{
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

void populate_global_names() {
	char buff[MAX_STR_CONST];
	char *global_prefix = "%ydbocto";

	snprintf(buff, MAX_STR_CONST, "^%sschema", global_prefix);
	buff[MAX_STR_CONST - 1] = '\0';
	config->global_names.schema = malloc(strlen(buff) + 1);
	strcpy(config->global_names.schema, buff);

	snprintf(buff, MAX_STR_CONST, "%ssession", global_prefix);
	buff[MAX_STR_CONST - 1] = '\0';
	config->global_names.session = malloc(strlen(buff) + 1);
	strcpy(config->global_names.session, buff);

	snprintf(buff, MAX_STR_CONST, "%scursor", global_prefix);
	buff[MAX_STR_CONST - 1] = '\0';
	config->global_names.cursor = malloc(strlen(buff) + 1);
	strcpy(config->global_names.cursor, buff);

	snprintf(buff, MAX_STR_CONST, "^%socto", global_prefix);
	buff[MAX_STR_CONST - 1] = '\0';
	config->global_names.octo = malloc(strlen(buff) + 1);
	strcpy(config->global_names.octo, buff);
	config->global_names.raw_octo = &config->global_names.octo[1];

	snprintf(buff, MAX_STR_CONST, "^%sxref", global_prefix);
	buff[MAX_STR_CONST - 1] = '\0';
	config->global_names.xref = malloc(strlen(buff) + 1);
	strcpy(config->global_names.xref, buff);
	config->global_names.raw_xref = &config->global_names.xref[1];
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
	int			status, i;
	unsigned int		c, zro_buf_size = ZRO_INIT_ALLOC;
	config_t		*config_file;
	config_setting_t	*ydb_settings, *cur_ydb_setting;
	const char		*item_name, *item_value;
	char			*default_octo_conf, buff[OCTO_PATH_MAX];
	char			*homedir, *ydb_dist, *zro_buf, ydb_errbuf[2048], *tmp_buf;
	ydb_buffer_t		zroutines, dollar_zro;
	DIR			*dir;

	const char *verbosity;
	int verbosity_int;

	config = malloc(sizeof(OctoConfig));
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

	default_octo_conf = malloc(octo_conf_default_len + 1);
	memcpy(default_octo_conf, octo_conf_default, octo_conf_default_len);
	default_octo_conf[octo_conf_default_len] = '\0';

	config_read_string(config_file, default_octo_conf);

	// Load config file
	if (NULL == config->config_file_name) {
		ydb_dist = getenv("ydb_dist");
		if (ydb_dist != NULL) {
			c = snprintf(buff, sizeof(buff), "%s/plugin/etc/%s", ydb_dist, OCTO_CONF_FILE_NAME);
			if (c < sizeof(buff))
			{
				assert('\0' == buff[c]);
				merge_config_file(buff, config_file);
			}
			/* else : snprintf output was truncated. Ignore this conf file and try other conf files. */
		}
		homedir = getenv("HOME");
		if (homedir != NULL) {
			c = snprintf(buff, sizeof(buff), "%s/%s", homedir, OCTO_CONF_FILE_NAME);
			if (c < sizeof(buff))
			{
				assert('\0' == buff[c]);
				merge_config_file(buff, config_file);
			}
			/* else : snprintf output was truncated. Ignore this conf file and try other conf files. */
		}
		c = snprintf(buff, sizeof(buff), "./%s", OCTO_CONF_FILE_NAME);
		if (c < sizeof(buff))
		{
			assert('\0' == buff[c]);
			merge_config_file(buff, config_file);
		}
		/* else : snprintf output was truncated. Ignore this conf file and try other conf files. */
	} else {
		merge_config_file(config->config_file_name, config_file);
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
			FATAL(ERR_BAD_CONFIG, "verbosity");
		}
	} else if (config_lookup_int(config_file, "verbosity", &verbosity_int) == CONFIG_FALSE) {
		printf("Verbosity set to %s\n", verbosity);
		FATAL(ERR_BAD_CONFIG, "verbosity");
	}

	if (config_lookup_string(config_file, "octo_zroutines", (const char**) &tmp_buf) == CONFIG_TRUE) {
		/* prepend to the start of $zroutines */
		YDB_LITERAL_TO_BUFFER("$zroutines", &dollar_zro);
		zro_buf = malloc(zro_buf_size);
		c = strlen(tmp_buf);
		/* resize if ZRO_INIT_ALLOC is too small
		 * *2 to avoid a potential resize later */
		if (zro_buf_size < c + 2)	/* + 2 is for ' ' and '\0' */
		{
			zro_buf_size = (c + 1) * 2;
			free(zro_buf);
			zro_buf = malloc(zro_buf_size);
		}
		strncpy(zro_buf, tmp_buf, c);
		zro_buf[c] = ' ';
		c++;
		zroutines.buf_addr = zro_buf + c;
		zroutines.len_alloc = zro_buf_size - c - 1;
		zroutines.len_used = 0;
		status = ydb_get_s(&dollar_zro, 0, NULL, &zroutines);
		/* if the stack buffer isn't large enough allocate more and call ydb_get_s() again */
		if (YDB_ERR_INVSTRLEN == status){
			zro_buf_size = zroutines.len_used + c + 1;
			char *tmp;
			tmp = malloc(zro_buf_size);
			strncpy(tmp, zro_buf, c);
			free(zro_buf);
			zro_buf = tmp;
			zroutines.buf_addr = zro_buf + c;
			zroutines.len_alloc = zroutines.len_used;
			zroutines.len_used = 0;
			status = ydb_get_s(&dollar_zro, 0, NULL, &zroutines);
		}
		if (YDB_OK != status){
			ydb_zstatus(ydb_errbuf, sizeof(ydb_errbuf));
			FATAL(ERR_YOTTADB, ydb_errbuf);
		}
		/* back the pointer up to the start of the buffer */
		zroutines.buf_addr = zro_buf;
		zroutines.len_used += c;
		zroutines.len_alloc = zro_buf_size;
		status = ydb_set_s(&dollar_zro, 0, NULL, &zroutines);
		if (YDB_ERR_ZROSYNTAX == status || YDB_ERR_DLLNOOPEN == status){
			FATAL(ERR_BAD_CONFIG, "octo_zroutines");
		} else if (YDB_OK != status){
			ydb_zstatus(ydb_errbuf, sizeof(ydb_errbuf));
			FATAL(ERR_YOTTADB, ydb_errbuf);
		}
		assert(zroutines.len_alloc >= zroutines.len_used + 1);
		zro_buf[zroutines.len_used] = '\0';	/* null terminate env var */
		setenv("ydb_routines", zro_buf, TRUE);
	} else {
		/* otherwise just get $zroutines */
		YDB_LITERAL_TO_BUFFER("$zroutines", &dollar_zro);
		zro_buf = malloc(zro_buf_size);
		zroutines.buf_addr = zro_buf;
		zroutines.len_alloc = zro_buf_size;
		zroutines.len_used = 0;
		status = ydb_get_s(&dollar_zro, 0, NULL, &zroutines);
		/* if ZRO_INIT_ALLOC isn't large enough allocate more and call ydb_get_s() again */
		if (YDB_ERR_INVSTRLEN == status){
			zro_buf_size = zroutines.len_used + 1;
			free(zro_buf);
			zro_buf = malloc(zro_buf_size);
			zroutines.buf_addr = zro_buf;
			zroutines.len_alloc = zro_buf_size;
			zroutines.len_used = 0;
			status = ydb_get_s(&dollar_zro, 0, NULL, &zroutines);
		}
		if (YDB_OK != status){
			ydb_zstatus(ydb_errbuf, sizeof(ydb_errbuf));
			FATAL(ERR_YOTTADB, ydb_errbuf);
		}
	}
	/* check if buffer will need a resize with the padding */
	if (zroutines.len_alloc - 2 < zroutines.len_used) {
		char *tmp = malloc(zroutines.len_alloc + 2);
		strncpy(tmp, zro_buf, zroutines.len_used);
		free(zro_buf);
		zro_buf = tmp;
		zroutines.buf_addr = zro_buf;
	}
	/* trim leading white space */
	c = 0;
	status = TRUE;
	while (c < zroutines.len_used && status){
		switch(zroutines.buf_addr[c]){
			case ' ' :
				c++;
				break;
			default:
				status = FALSE;
				break;
		}
	}
	zroutines.buf_addr += c;
	zroutines.len_used -= c;
	c = 0;
	/* pad end of zroutines with a space to trigger the switch case */
	zroutines.buf_addr[zroutines.len_used] = ' ';
	zroutines.buf_addr[zroutines.len_used + 1] = '\0';
	zroutines.len_used += 2;
	struct stat statbuf;
	while ('\0' != zroutines.buf_addr[c]){
		switch(zroutines.buf_addr[c]){
			/* white space characters, and right paren are delimiters */
			case ' ' :
			case ')' :
				/* terminate everything read up to this point
				 * if last character is a '*' remove it
				 */
				if (1 < c){
					if ('*' == zroutines.buf_addr[c-1]){
						c--;
					}
				}
				/* check if this path is a directory
				 * if it is not move the start of the string to the next token
				 */
				zroutines.buf_addr[c] = '\0';
				status = stat(zroutines.buf_addr, &statbuf);
				/* if 0 == c don't emit an error since that is just the empty string
				 * if 2 == errno (file does not exist) don't emit an error just skip it
				 */
				if (0 != status && 2 != errno && 0 != c){
					FATAL(ERR_SYSCALL, "stat", errno, strerror(errno));
				}
				if (!S_ISDIR(statbuf.st_mode)){
					zroutines.buf_addr += (c+1);
					c = 0;
					break;
				}
				/* +1 for null terminator */
				config->tmp_dir = malloc(c + 1);
				/* cast to char* to avoid strcpy warning this is safe to do here */
				strncpy((char*) config->tmp_dir, zroutines.buf_addr, c + 1);
				break;
			case '(':
				/* if a '(' is found shift start of string */
				zroutines.buf_addr += (c+1);
				c = 0;
				break;
			default:
				c++;
				break;
		}
	}
	if (!config->tmp_dir){
		FATAL(ERR_BAD_ZROUTINES, NULL);
	}
	free(zro_buf);

	if (config_lookup_string(config_file, "rocto.address", &config->rocto_config.address)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.address");
	}
	if (config_lookup_int(config_file, "rocto.port", &config->rocto_config.port)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.port");
	}
	if (config_lookup_bool(config_file, "rocto.use_dns", &config->rocto_config.use_dns)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.use_dns");
	}
#	if YDB_TLS_AVAILABLE
	if (config_lookup_string(config_file, "tls.OCTOSERVER.cert", &config->rocto_config.ssl_cert_file)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "tls.OCTOSERVER.cert");
	}
	if (config_lookup_string(config_file, "tls.OCTOSERVER.key", &config->rocto_config.ssl_key_file)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "tls.OCTOSERVER.key");
	}
	if (config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.ssl_on");
	}

#	else
	if (config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.ssl_on");
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
		FATAL(ERR_SYSCALL, "opendir (config.tmp_dir)", errno, strerror(errno));
	}
	free(dir);

	config->page_size = sysconf(_SC_PAGESIZE);
	populate_global_names();
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

	ydb_int64_t ci_return = 1;
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
