/****************************************************************
 *								*
 * Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	*
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
int parse_startup_flags(int argc, char **argv, char **config_file_name);

typedef struct RoctoConfig {
	int	    port;
	int	    ssl_on;
	int	    ssl_required;
	const char *address;
	const char *ssl_cert_file;
	const char *ssl_key_file;
	int	    use_dns;
	boolean_t   tcp_delay;
} RoctoConfig;

// Contains YottaDB global and local variable names used internally by Octo.
// Each buffer includes an extra byte for the null terminator.
typedef struct {
	char  schema[YDB_MAX_IDENT + 1];
	char  session[YDB_MAX_IDENT + 1];
	char  cursor[YDB_MAX_IDENT + 1];
	char  octo[YDB_MAX_IDENT + 1];
	char  xref[YDB_MAX_IDENT + 1];
	char  loadedschemas[YDB_MAX_IDENT + 1];
	char *raw_octo;
	char *raw_xref;
} GlobalNames;

typedef enum {
	EMULATION_UNSET,
	POSTGRES,
	MYSQL,
} DatabaseEmulation;

typedef enum {
	TABLETYPE_READWRITE,
	TABLETYPE_READONLY,
} tabletype_t;

typedef struct OctoConfig {
	enum VERBOSITY_LEVEL verbosity_level;
	DatabaseEmulation    database_emulation;
	RoctoConfig	     rocto_config;
	GlobalNames	     global_names;
	const char *	     plan_src_dir; /* the directory where _ydbocto*.m plans are generated */
	const char *	     plan_obj_dir; /* the directory where _ydbocto*.o object files are generated */
	boolean_t	     is_tty, is_rocto;
	config_t *	     config_file;
	pid_t		     process_id;
	boolean_t	     dry_run;
	boolean_t	     octo_print_flag_specified; /* TRUE if "-p" or "--print-sql-query" was specified in octo startup */
	boolean_t	     octo_print_query;		/* prints each query in "octo" (not "rocto") as it is executed.
							 * Does nothing if "config->is_tty" is TRUE (readline already
							 * prints query in that case).
							 */
	int	  plan_id, page_size;
	boolean_t allow_schema_changes;
	boolean_t readwrite;
	boolean_t in_auto_upgrade_binary_table_definition; /* TRUE for a short window when octo/rocto startup detects it is
							    * time to auto upgrade the binary table definitions.
							    */
	boolean_t in_auto_upgrade_binary_view_definition;  /* TRUE for a short window when octo/rocto startup detects it is
							    * time to auto upgrade the binary view definitions.
							    */
	boolean_t in_auto_load_octo_seed;		   /* TRUE for a short window when octo/rocto startup detects it is
							    * time to auto load the octo-seed.sql and octo-seed.zwr files.
							    */
	boolean_t is_auto_upgrade_octo929;		   /* TRUE for a short window when octo/rocto startup detects it is
							    * time to auto upgrade due to YDBOcto#929 (case insensitive names
							    * of table/column/function/view are stored in lower case).
							    */
	char octo929_sqlfile[48];			   /* Holds the file name where we write the CREATE TABLE/CREATE FUNCTION
							    * commands during the special YDBOcto#929 auto upgrade.
							    */
	FILE *octo929_sqlfile_stream;			   /* File used to write CREATE TABLE/CREATE FUNCTION commands during
							    * special YDBOcto#929 auto upgrade.
							    */
	tabletype_t default_tabletype;			   /* type of table (readonly vs readwrite) assigned to CREATE TABLE
							    * if not explicitly specified.
							    */
	const char *octo_history;			   /* User supplied and also final parsed location of history */
	int	    octo_history_max_length;		   /* User supplied */
#ifndef NDEBUG
	boolean_t seedreload; /* Only used for testing autoload */
#endif
} OctoConfig;

extern OctoConfig *config;

#endif
