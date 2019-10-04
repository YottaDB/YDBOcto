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

char *similar_to_regex(const char *src) {
	char *ret, *d, *end;
	const char *c;

	ret = octo_cmalloc(memory_chunks, MAX_STR_CONST);
	end = ret + MAX_STR_CONST;
	d = ret;
	c = src;

	*d++ = '^';

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
				/* this gets a bit abusive here
				 * Posix treats all of these meta-characters as off by default
				 * but SIMILAR TO treats them as on by default
				 * so if we find them escaped we need to copy them over as literals
				 * '%' and '_' also need to be copied as literals
				 * '\\', '*', '[', ']' are on by default so in this case keep the escape character
				 */
				switch (*(c + 1)) {
					case '%':
					case '_':
					case '|':
					case '+':
					case '?':
					case '{':
					case '}':
					case '(':
					case ')':
						c++;
						*d++ = *c;
						break;
					case '\\':
					case '*':
					case '[':
					case ']':
						*d++ = '\\';
						c++;
						*d++ = *c;
						break;
					default:
						break;
				}
				break;
			/* escape these as they should not be parsed by the regex engine */
			case '.':
				*d++ = '\\';
				*d++ = '.';
				break;
			/* escape these characters because they should be parsed by the regex engine */
			case '|':
			case '+':
			case '?':
			case '{':
			case '}':
			case '(':
			case ')':
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
