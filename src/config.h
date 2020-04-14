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

#ifndef CONFIG_H
#define CONFIG_H

#include <libyottadb.h>
#include <libconfig.h>
#include <sys/types.h>

#include "errors.h"
#include "octo_types.h"

int octo_init(int argc, char **argv);
int parse_startup_flags(int argc, char **argv, char *config_file_name);

typedef struct RoctoConfig {
	int port;
	int ssl_on;
	const char *address;
	const char *ssl_cert_file;
	const char *ssl_key_file;
	int use_dns;
} RoctoConfig;

// Contains YottaDB global and local variable names used internally by Octo.
// Each buffer includes an extra byte for the null terminator.
typedef struct {
	char schema[YDB_MAX_IDENT+1];
	char session[YDB_MAX_IDENT+1];
	char cursor[YDB_MAX_IDENT+1];
	char octo[YDB_MAX_IDENT+1];
	char xref[YDB_MAX_IDENT+1];
	char loadedschemas[YDB_MAX_IDENT+1];
	char *raw_octo;
	char *raw_xref;
} GlobalNames;

typedef struct OctoConfig {
	enum VERBOSITY_LEVEL	verbosity_level;
	RoctoConfig		rocto_config;
	GlobalNames		global_names;
	const char		*tmp_dir;
	boolean_t		is_tty, is_rocto;
	config_t		*config_file;
	pid_t			process_id;
	int			dry_run, plan_id, page_size, allow_schema_changes;
} OctoConfig;

OctoConfig *config;

#endif
