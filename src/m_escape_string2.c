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

#include "octo.h"
#include "octo_types.h"

int m_escape_string2(char *buffer, int buffer_len, char *string) {
	int i=0;
	char *c = string;
	char *b = buffer;
	while(*c != '\0' && i < buffer_len) {
		switch(*c) {
		case '"':
			*b++ = '"';
			*b++ = '"';
			i += 2;
			break;
		default:
			*b++ = *c;
			i++;
		}
		c++;
	}
	*b = '\0';
	return i;
}
