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

#include <assert.h>

#include "octo_types.h"
#include "template_helpers.h"

void tmpl_print_dots(char **global_buffer, int *buffer_len, int *buffer_index, int dots) {
	int i;
	char *buffer;

	/* dots * 2 as a dot followed by a space is printed */
	while((dots * 2) >= (*buffer_len - *buffer_index)){
		resize_tmpl_buffer(global_buffer, buffer_len, buffer_index);
	}
	buffer = *global_buffer;
	for(i=0; i < dots; i++) {
		buffer[(*buffer_index)++] = '.';
		buffer[(*buffer_index)++] = ' ';
	}
	return;
}
