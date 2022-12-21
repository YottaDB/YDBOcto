/****************************************************************
 *								*
 * Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <stdio.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

#define SLASH_D_QUERY_STR \
	"				\
	 SELECT n.nspname                              AS \"Schema\",	\
	        c.relname                              AS \"Name\",	\
	        CASE c.relkind						\
	          WHEN 'r' THEN 'table'					\
	          WHEN 'v' THEN 'view'					\
	          WHEN 'm' THEN 'materialized view'			\
	          WHEN 'i' THEN 'index'					\
	          WHEN 'S' THEN 'sequence'				\
	          WHEN 's' THEN 'special'				\
	          WHEN 'f' THEN 'foreign table'				\
	          WHEN 'p' THEN 'table'					\
	          WHEN 'I' THEN 'index'					\
	        end                                    AS \"Type\",	\
	        pg_catalog.Pg_get_userbyid(c.relowner) AS \"Owner\"	\
	 FROM   pg_catalog.pg_class c					\
	        LEFT JOIN pg_catalog.pg_namespace n			\
	               ON n.oid = c.relnamespace			\
	 WHERE  c.relkind IN ( 'r', 'p', 'v', 'm',			\
	                       'S', 'f', '' )				\
	        AND n.nspname <> 'pg_catalog'				\
	        AND n.nspname <> 'information_schema'			\
	        AND n.nspname !~ '^pg_toast'				\
	        AND pg_catalog.Pg_table_is_visible(c.oid)		\
	 ORDER  BY 1, 2;						\
	 "

/* Returns SqlStatement for \d query */
SqlStatement *get_display_relation_query_stmt(ParseContext *parse_context) {
	char display_all_relation_query_str[] = SLASH_D_QUERY_STR;
	// Save parse_line() related things
	boolean_t save_is_tty = config->is_tty;
	int	  save_cur_input_index = cur_input_index;
	int	  save_old_input_index = old_input_index;
	char *	  save_input_buffer_combined = input_buffer_combined;
	int	  save_leading_spaces = leading_spaces;
	int (*save_cur_input_more)();
	assert(cur_input_more == &readline_get_more);
	save_cur_input_more = cur_input_more;

	/* It is possible "eof_hit" is set to a value other than EOF_NONE in case `\d` was the last query
	 * when octo is reading input from a file. In that case, we want to process the query in
	 * "display_all_relation_query_str" without getting affected by "eof_hit" hence the save/restore below.
	 */
	int save_eof_hit = eof_hit;
	eof_hit = EOF_NONE;

	// Set new parse_line parameters
	config->is_tty = 0;
	cur_input_index = 0;
	old_input_index = 0;
	input_buffer_combined = display_all_relation_query_str;
	cur_input_more = no_more;

	// Invoke parse_line
	SqlStatement *result = parse_line(parse_context);

	// Restore parse_line parameters
	config->is_tty = save_is_tty;
	cur_input_index = save_cur_input_index;
	old_input_index = save_old_input_index;
	input_buffer_combined = save_input_buffer_combined;
	cur_input_more = save_cur_input_more;
	leading_spaces = save_leading_spaces;
	eof_hit = save_eof_hit;

	return result;
}
