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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "octo.h"
#include "octo_types.h"

#define EXPAND_BUFFER_IF_NEEDED(NEXT_INDEX, BUFFER_LEN, BUFFER, CHAR)                                                   \
	{                                                                                                               \
		if (NEXT_INDEX >= *BUFFER_LEN) {                                                                        \
			char *tmp;                                                                                      \
			int   new_size;                                                                                 \
                                                                                                                        \
			new_size = ((NEXT_INDEX >= (*BUFFER_LEN * 2)) ? (NEXT_INDEX + 1) : (*BUFFER_LEN * 2));          \
			tmp = (char *)malloc(sizeof(char) * new_size);                                                  \
			/* Copy up to the current index, leaving space for the next to populated after the expansion */ \
			memcpy(tmp, *BUFFER, NEXT_INDEX - 1);                                                           \
			free(*BUFFER);                                                                                  \
			*BUFFER = tmp;                                                                                  \
			*BUFFER_LEN = new_size;                                                                         \
			CHAR = *BUFFER + NEXT_INDEX - 1;                                                                \
		}                                                                                                       \
	}

int m_escape_string2(char **buffer, int *buffer_len, char *string) {
	int   i = 0;
	char *c = string;
	char *b = *buffer;
	while (*c != '\0') {
		switch (*c) {
		case '"':
			i++;
			EXPAND_BUFFER_IF_NEEDED(i, buffer_len, buffer, b);
			*b++ = '"';
			i++;
			EXPAND_BUFFER_IF_NEEDED(i, buffer_len, buffer, b);
			*b++ = '"';
			break;
		default:
			i++;
			EXPAND_BUFFER_IF_NEEDED(i, buffer_len, buffer, b);
			*b++ = *c;
			break;
		}
		c++;
	}
	i++;
	EXPAND_BUFFER_IF_NEEDED(i, buffer_len, buffer, b);
	assert(b < (*buffer + *buffer_len));
	*b = '\0';
	return i;
}
