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

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <assert.h>
#include <string.h>

#include <libyottadb.h>
#include <gtmxc_types.h>

#include "octo.h"
#include "octo_types.h"
#include "physical_plan.h"
#include "parser.h"
#include "lexer.h"

int parse_startup_flags(int argc, char **argv) {
	int c;

	optind = 1;

	/* Parse input parameters */
	while (1)
	{
		static struct option long_options[] =
		{
			{"verbose", optional_argument, NULL, 'v'},
			{"dry-run", no_argument, NULL, 'd'},
			{"input-file", required_argument, NULL, 'f'},
			{"config-file", required_argument, NULL, 'c'},
			{"keep-temporary", required_argument, NULL, 'k'},
			{0, 0, 0, 0}
		};
		int option_index = 0;

		c = getopt_long(argc, argv, "kvdf:t:c:", long_options, &option_index);
		if(c == -1)
			break;

		switch(c)
		{
		case 0:
			if(long_options[option_index].flag != 0)
				break;
			break;
		case 'v':
			if(optarg) {
				c = atoi(optarg);
				if(c > FATAL || c < TRACE) {
					ERROR(CUSTOM_ERROR, "Invalid value specified for --verbose");
					return 1;
				}
				config->record_error_level = FATAL - c;
			} else {
				config->record_error_level = config->record_error_level > TRACE
				                             ? config->record_error_level - 1 : config->record_error_level;
			}
			break;
		case 'd':
			config->dry_run = TRUE;
			break;
		case 'f':
			inputFile = fopen(optarg, "r");
			if (inputFile == NULL)
			{
				FATAL(ERR_FILE_NOT_FOUND, optarg);
			}
			break;
		case 'c':
			config->config_file_name = optarg;
			break;
		case 'k':
			config->auto_clean_tables = FALSE;
			break;
		default:
			ERROR(CUSTOM_ERROR, "Uknown argument");
			return 1;
		}
	}
	return 0;
}
