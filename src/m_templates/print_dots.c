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

int print_dots(char *buffer, int buffer_length, int dots) {
	int i;
	char *buff_ptr = buffer;

	assert(buffer_length > dots);
	for(i=0; i < dots; i++) {
		*buff_ptr++ = '.';
		*buff_ptr++ = ' ';
	}
	return buff_ptr - buffer;
}
