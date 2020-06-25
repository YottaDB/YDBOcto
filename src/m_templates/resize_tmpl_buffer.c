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

#include "octo_types.h"
#include "template_helpers.h"

void resize_tmpl_buffer(char **global_buffer, int *buffer_len, int *buffer_index) {
	*buffer_len *= 2;
	char *tmp = calloc(*buffer_len, sizeof(char));
	memcpy(tmp, *global_buffer, *buffer_index);
	free(*global_buffer);
	*global_buffer = tmp;
	return;
}
