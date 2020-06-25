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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "memory_chunk.h"

char *m_escape_string(const char *string) {
	char *	    buf, *e;
	const char *c;
	int	    len = strlen(string);
	buf = octo_cmalloc(memory_chunks, 2 * len);
	for (c = string, e = buf; *c != '\0'; c++) {
		if (*c == '"') {
			*e++ = '"';
			*e++ = '"';
		} else
			*e++ = *c;
	}
	*e = '\0';
	return buf;
}

char *m_unescape_string(const char *string) {
	char *	    buf, *e;
	const char *c;
	int	    len = strlen(string), quote_count = 0;
	buf = octo_cmalloc(memory_chunks, 2 * len);
	for (c = string, e = buf; *c != '\0'; c++) {
		if (*c == '"') {
			if (quote_count == 1) {
				quote_count = 0;
				*e++ = '"';
			} else {
				quote_count++;
			}
		} else {
			*e++ = *c;
			assert(quote_count == 0);
		}
	}
	*e = '\0';
	return buf;
}
