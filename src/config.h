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

#ifndef CONFIG_H
#define CONFIG_H

#include <libyottadb.h>
#include <libconfig.h>

#include "errors.h"

int octo_init(int argc, char **argv);
int parse_startup_flags(int argc, char **argv);

typedef struct RoctoConfig {
	int port;
	int ssl_on;
	const char *address;
	const char *ssl_cert_file;
	const char *ssl_key_file;
	int use_dns;
} RoctoConfig;

typedef struct {
	char *schema, *session, *cursor, *octo, *xref;
	char *raw_schema, *raw_session, *raw_cursor, *raw_octo, *raw_xref;
} GlobalNames;

typedef struct OctoConfig {
	config_t config_file;
	RoctoConfig rocto_config;
	GlobalNames global_names;
	enum ERROR_LEVEL record_error_level;
	int dry_run, plan_id, auto_clean_tables;
	const char *tmp_dir, *global_directory, *config_file_name, *global_prefix;
	ydb_buffer_t zgbldir, octo_gbldir, prev_gbldir;
	int is_tty, page_size;
} OctoConfig;

OctoConfig *config;

#endif
