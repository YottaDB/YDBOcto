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
#include <string.h>

#include "octo.h"
#include "octo_types.h"

char *like_to_regex(const char *src) {
	char *ret, *d, *end;
	const char *c;

	ret = octo_cmalloc(memory_chunks, MAX_STR_CONST);
	end = ret + MAX_STR_CONST;
	d = ret;
	c = src;

	while (*c != '\0' && d < end) {
		switch (*c) {
			/* convert like meta characters to regex */
			case '%':
				*d++ = '.';
				*d++ = '*';
				break;
			case '_':
				*d++ = '.';
				break;
			case '\\':
				/* if it is a '%' or '_' copy them as a literal and move forward
				 * '\\' needs to have both characters in the regex engine
				 * otherwise just skip the next character
				 */
				if (('%' == *(c + 1)) || ('_' == *(c + 1))) {
					c++;
					*d++ = *c;
					break;
				} else if ('\\' == *(c + 1)) {
					*d++ = '\\';
					*d++ = '\\';
					c++;
				}
				break;
			/* escape these as they should not be parsed by the regex engine */
			case '.':
			case '*':
			case '[':
			case ']':
				*d++ = '\\';
				*d++ = *c;
				break;
			default:
				*d++ = *c;
				break;
		}
		c++;
	}

	*d++ = '$';
	*d = '\0';

	return ret;
}
