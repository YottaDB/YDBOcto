/****************************************************************
 *								*
 * Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	*
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
#include <dirent.h>
#include <errno.h>

#include <openssl/conf.h>
#include <openssl/evp.h>
#include <openssl/err.h>

#include "octo.h"
#include "octo_types.h"

// Read binary file with default config settings
#include "default_octo_conf.h"

void merge_config_file_helper(config_setting_t *a, config_setting_t *b);

void merge_config_file(const char *path, config_t *config_file) {
	const char *error_message, *error_file;
	int error_line;
	config_t new_config_file;
	config_setting_t *a_root, *b_root;
	if(access(path, F_OK) == -1) {
		// File not found or no access; skip it
		return;
	}
	config_init(&new_config_file);
	if(config_read_file(&new_config_file, path) == CONFIG_FALSE) {
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
	while(TRUE) {
		b_setting = config_setting_get_elem(b, b_index);
		if(b_setting == NULL)
			break;
		setting_name = config_setting_name(b_setting);
		setting_type = config_setting_type(b_setting);
		if(config_setting_is_group(b_setting) || config_setting_is_array(b_setting)
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
			}
			if(t_setting == NULL) {
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

	YDB_LITERAL_TO_BUFFER("$ZGBLDIR", &config->zgbldir);
	YDB_MALLOC_BUFFER(&config->prev_gbldir, MAX_STR_CONST);
	YDB_STRING_TO_BUFFER((char*)config->global_directory, &config->octo_gbldir);
	snprintf(buff, MAX_STR_CONST, "^%sschema", config->global_prefix);
	buff[MAX_STR_CONST - 1] = '\0';
	config->global_names.schema = malloc(strlen(buff) + 1);
	strcpy(config->global_names.schema, buff);
	config->global_names.raw_schema = &config->global_names.schema[1];

	snprintf(buff, MAX_STR_CONST, "^%ssession", config->global_prefix);
	buff[MAX_STR_CONST - 1] = '\0';
	config->global_names.session = malloc(strlen(buff) + 1);
	strcpy(config->global_names.session, buff);
	config->global_names.raw_session = &config->global_names.session[1];

	snprintf(buff, MAX_STR_CONST, "^%scursor", config->global_prefix);
	buff[MAX_STR_CONST - 1] = '\0';
	config->global_names.cursor = malloc(strlen(buff) + 1);
	strcpy(config->global_names.cursor, buff);
	config->global_names.raw_cursor = &config->global_names.cursor[1];

	snprintf(buff, MAX_STR_CONST, "^%socto", config->global_prefix);
	buff[MAX_STR_CONST - 1] = '\0';
	config->global_names.octo = malloc(strlen(buff) + 1);
	strcpy(config->global_names.octo, buff);
	config->global_names.raw_octo = &config->global_names.octo[1];

	snprintf(buff, MAX_STR_CONST, "^%sxref", config->global_prefix);
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

int octo_init(int argc, char **argv, int scan_tables) {
	int c, status, i;
	ydb_buffer_t schema_global, table_name_buffer, table_create_buffer, null_buffer;
	ydb_buffer_t z_status, z_status_value;
	SqlStatement *result = 0;
	SqlTable *table, *t_table;
	config_t *config_file;
	config_setting_t *ydb_settings, *cur_ydb_setting;
	const char *item_name, *item_value;
	char *default_octo_conf, buff[MAX_STR_CONST];
	char *home;
	DIR *dir;

	const char *verbosity;
	int verbosity_int;

	config = malloc(sizeof(OctoConfig));
	memset(config, 0, sizeof(OctoConfig));
	config_file = &config->config_file;

	// If OCTO_SETTINGS env is set, we previously parsed the config and stashed it
	//   in environment variables; load from there

	// Parse the startup flags; we will have to do this again after we read the config
	status = parse_startup_flags(argc, argv);
	if(status)
		return status;

	// Search for the config file in /etc/octo.conf, ~/.octo.conf, and ./.octo.conf
	config_init(config_file);

	default_octo_conf = malloc(octo_conf_default_len + 1);
	memcpy(default_octo_conf, octo_conf_default, octo_conf_default_len);
	default_octo_conf[octo_conf_default_len] = '\0';

	config_read_string(config_file, default_octo_conf);

	// Load config file
	if(config->config_file_name == NULL) {
		merge_config_file("/etc/octo.conf", config_file);
		home = getenv("HOME");
		if(home != NULL) {
		c = snprintf(buff, MAX_STR_CONST, "%s/.octo.conf", home);
		buff[c] = '\0';
		merge_config_file(buff, config_file);
		}
		merge_config_file(".octo.conf", config_file);
	} else {
		merge_config_file(config->config_file_name, config_file);
	}
	free(default_octo_conf);

	if(config_lookup_string(config_file, "verbosity", &verbosity) == CONFIG_TRUE) {
		if(strcmp(verbosity, "TRACE") == 0) {
			verbosity_int = TRACE;
		} else if(strcmp(verbosity, "INFO") == 0) {
			verbosity_int = INFO;
		} else if(strcmp(verbosity, "DEBUG") == 0) {
			verbosity_int = DEBUG;
		} else if(strcmp(verbosity, "WARNING") == 0) {
			verbosity_int = WARNING;
		} else if(strcmp(verbosity, "ERROR") == 0) {
			verbosity_int = ERROR;
		} else if(strcmp(verbosity, "FATAL") == 0) {
			verbosity_int = FATAL;
		} else {
			FATAL(ERR_BAD_CONFIG, "verbosity");
		}
	} else if(config_lookup_int(config_file, "verbosity", &verbosity_int) == CONFIG_FALSE) {
		printf("Verbosity set to %s\n", verbosity);
		FATAL(ERR_BAD_CONFIG, "verbosity");
	}

	if(config_lookup_string(config_file, "routine_cache", &config->tmp_dir) == CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "routine_cache");
	}

	if(config_lookup_string(config_file, "octo_global_prefix", &config->global_prefix)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "octo_global_prefix");
	}
	if(config_lookup_bool(config_file, "auto_clean_tables", &config->auto_clean_tables)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "auto_clean_tables");
	}
	if(config_lookup_string(config_file, "rocto.address", &config->rocto_config.address)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.address");
	}
	if(config_lookup_int(config_file, "rocto.port", &config->rocto_config.port)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.port");
	}
#if YDB_TLS_AVAILABLE
	if(config_lookup_string(config_file, "tls.DEVELOPMENT.cert", &config->rocto_config.ssl_cert_file)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "tls.DEVELOPMENT.cert");
	}
	if(config_lookup_string(config_file, "tls.DEVELOPMENT.key", &config->rocto_config.ssl_key_file)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "tls.DEVELOPMENT.key");
	}
	if(config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.ssl_on");
	}
#else
	if(config_lookup_bool(config_file, "rocto.ssl_on", &config->rocto_config.ssl_on)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.ssl_on");
	}
	if (config->rocto_config.ssl_on) {
		FATAL(ERR_BAD_CONFIG, "rocto.ssl_on set, but YottaDB TLS plugin not installed");
	}
#endif
	// Read in YDB settings
	ydb_settings = config_lookup(config_file, "yottadb");
	if(ydb_settings != NULL && config_setting_is_group(ydb_settings) == CONFIG_TRUE) {
		i = 0;
		while((cur_ydb_setting = config_setting_get_elem(ydb_settings, i)) != NULL) {
			item_name = config_setting_name(cur_ydb_setting);
			item_value = config_setting_get_string(cur_ydb_setting);
			if(getenv(item_name) == NULL) {
				setenv(item_name, item_value, 0);
			}
			i++;
		}
	}

	// If users don't provide a global directory setting, use the YottaDB setting
	if(config_lookup_string(config_file, "octo_global_directory", &config->global_directory)
			== CONFIG_FALSE) {
		if(getenv("ydb_gbldir") != NULL) {
			config_setting_t *new_setting = config_setting_add(config_root_setting(config_file),
					"octo_global_directory", CONFIG_TYPE_STRING);
			config_setting_set_string(new_setting, getenv("ydb_gbldir"));
		} else if(getenv("gtmgbldir") != NULL) {
			config_setting_t *new_setting = config_setting_add(config_root_setting(config_file),
					"octo_global_directory", CONFIG_TYPE_STRING);
			config_setting_set_string(new_setting, getenv("gtmgbldir"));
		}
		if(config_lookup_string(config_file, "octo_global_directory", &config->global_directory)
				== CONFIG_FALSE) {
			FATAL(ERR_BAD_CONFIG, "octo_global_directory");
		}
	}

	config->record_error_level = verbosity_int;
	//config->dry_run = FALSE;
	err_buffer = stderr;

	// Reparse the startup flags to ensure they overwrite the config files
	status = parse_startup_flags(argc, argv);
	if(status)
		return status;

	// Verify that the directory exists, or issue an error
	dir = opendir(config->tmp_dir);
	if(dir == NULL) {
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
	cur_input_index = 0;
	cur_input_more = &no_more;
	eof_hit = 0;

	if(!scan_tables)
		return 0;

	// Load existing tables
	YDB_MALLOC_BUFFER(&table_name_buffer, MAX_STR_CONST);
	YDB_MALLOC_BUFFER(&table_create_buffer, MAX_STR_CONST);
	SWITCH_TO_OCTO_GLOBAL_DIRECTORY();

	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	YDB_LITERAL_TO_BUFFER("", &null_buffer);
	memory_chunks = alloc_chunk(MEMORY_CHUNK_SIZE);
	do {
		status = ydb_subscript_next_s(&schema_global, 1, &table_name_buffer, &table_name_buffer);
		if(status == YDB_ERR_NODEEND) {
			break;
		}

		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		if(table_name_buffer.len_used == 0)
			break;
		status = ydb_get_s(&schema_global, 1, &table_name_buffer, &table_create_buffer);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		table_create_buffer.buf_addr[table_create_buffer.len_used] = '\0';
		INFO(CUSTOM_ERROR, "Running command %s\n", table_create_buffer.buf_addr);
		eof_hit = 0;
		result = parse_line(table_create_buffer.buf_addr);
		if(result == NULL) {
			table_name_buffer.buf_addr[table_name_buffer.len_used] = '\0';
			WARNING(ERR_FAILED_TO_PARSE_SCHEMA, table_name_buffer.buf_addr);
			continue;
		}
		UNPACK_SQL_STATEMENT(table, result, table);
		if(definedTables == NULL) {
			definedTables = table;
			dqinit(definedTables);
		} else {
			dqinsert(definedTables, table, t_table);
		}
		result = NULL;
	} while(1);

	YDB_FREE_BUFFER(&table_create_buffer);
	YDB_FREE_BUFFER(&table_name_buffer);

	return 0;
}
