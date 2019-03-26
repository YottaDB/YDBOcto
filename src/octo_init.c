/* Copyright (C) 2018 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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

void merge_config_file(const char *path, config_t *config_file) {
	const char *error_message, *error_file;
	int error_line;
	if(access(path, F_OK) == -1) {
		// File not found or no access; skip it
		return;
	}
	if(config_read_file(config_file, path) == CONFIG_FALSE) {
		error_message = config_error_text(config_file);
		error_file = config_error_file(config_file);
		error_line = config_error_line(config_file);
		FATAL(ERR_PARSING_CONFIG, error_file,
				error_line,
				error_message);
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
	int c, error = 0, status, i;
	ydb_buffer_t schema_global, table_name_buffer, table_create_buffer, null_buffer;
	ydb_buffer_t cursor_global, cursor_exe_global[3];
	ydb_buffer_t z_status, z_status_value;
	SqlStatement *result = 0;
	SqlTable *table, *t_table;
	config_t *config_file;
	config_setting_t *ydb_settings, *cur_ydb_setting, *setting, *config_root, *cur_root;
	const char *item_name, *item_value;
	char *global_dir;
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

	// Add default settings
	cur_root = config_root = config_root_setting(config_file);
	setting = config_setting_add(cur_root, "verbosity", CONFIG_TYPE_STRING);
	if(config_setting_set_string(setting, "WARNING") == CONFIG_FALSE) {
		assert(FALSE);
	}
	setting = config_setting_add(cur_root, "routine_cache", CONFIG_TYPE_STRING);
	if(config_setting_set_string(setting, "./") == CONFIG_FALSE) {
		assert(FALSE);
	}
	setting = config_setting_add(cur_root, "octo_global_directory", CONFIG_TYPE_STRING);
	global_dir = getenv("ydb_gbldir");
	if(global_dir == NULL) {
		global_dir = "mumps.gld";
	}
	if(config_setting_set_string(setting, global_dir) == CONFIG_FALSE) {
		assert(FALSE);
	}
	setting = config_setting_add(cur_root, "octo_global_prefix", CONFIG_TYPE_STRING);
	if(config_setting_set_string(setting, "%ydbocto") == CONFIG_FALSE) {
		assert(FALSE);
	}
	// Rocto setting
	setting = config_setting_add(config_root, "rocto", CONFIG_TYPE_GROUP);
	if(setting == NULL) {
		assert(FALSE);
	}
	cur_root = setting;
	setting = config_setting_add(cur_root, "address", CONFIG_TYPE_STRING);
	if(config_setting_set_string(setting, "127.0.0.1") == CONFIG_FALSE) {
		assert(FALSE);
	}
	setting = config_setting_add(cur_root, "port", CONFIG_TYPE_INT);
	if(config_setting_set_int(setting, 1337) == CONFIG_FALSE) {
		assert(FALSE);
	}

	// Load config file
	if(config->config_file_name == NULL) {
		merge_config_file("/etc/octo.conf", config_file);
		merge_config_file("~/.octo.conf", config_file);
		merge_config_file(".octo.conf", config_file);
	} else {
		merge_config_file(config->config_file_name, config_file);
	}

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
		FATAL(ERR_BAD_CONFIG, "verbosity");
	}

	if(config_lookup_string(config_file, "routine_cache", &config->tmp_dir) == CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "routine_cache");
	}

	if(config_lookup_string(config_file, "octo_global_directory", &config->global_directory)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "octo_global_directory");
	}
	if(config_lookup_string(config_file, "octo_global_prefix", &config->global_prefix)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "octo_global_directory");
	}
	if(config_lookup_string(config_file, "rocto.address", &config->rocto_config.address)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.address");
	}
	if(config_lookup_int(config_file, "rocto.port", &config->rocto_config.port)
			== CONFIG_FALSE) {
		FATAL(ERR_BAD_CONFIG, "rocto.port");
	}

	// Read in YDB settings
	ydb_settings = config_lookup(config_file, "yottadb");
	if(ydb_settings != NULL && config_setting_is_group(ydb_settings) == CONFIG_TRUE) {
		i = 0;
		while((cur_ydb_setting = config_setting_get_elem(ydb_settings, i)) != NULL) {
			item_name = config_setting_name(cur_ydb_setting);
			item_value = config_setting_get_string(cur_ydb_setting);
			setenv(item_name, item_value, 0);
			i++;
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
		FATAL(ERR_SYSCALL, "opendir (config.tmp_dir)", errno);
	}
	free(dir);

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
	table_name_buffer.buf_addr = malloc(MAX_STR_CONST);
	table_name_buffer.len_used = 0;
	table_name_buffer.len_alloc = MAX_STR_CONST;
	table_create_buffer.buf_addr = malloc(MAX_STR_CONST);
	table_create_buffer.len_used = 0;
	table_create_buffer.len_alloc = MAX_STR_CONST;
	SWITCH_TO_OCTO_GLOBAL_DIRECTORY();

	YDB_STRING_TO_BUFFER(config->global_names.schema, &schema_global);
	YDB_LITERAL_TO_BUFFER("", &null_buffer);
	do {
		status = ydb_subscript_next_s(&schema_global, 1, &table_name_buffer, &table_name_buffer);
		if(status == YDB_ERR_NODEEND) {
			break;
		}

		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		if(table_name_buffer.len_used == 0)
			break;
		ydb_get_s(&schema_global, 1, &table_name_buffer, &table_create_buffer);
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
		free(result);
		result = NULL;
	} while(1);

	free(table_create_buffer.buf_addr);
	free(table_name_buffer.buf_addr);

	return 0;
}
