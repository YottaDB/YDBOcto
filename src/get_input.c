/****************************************************************
 *								*
 * Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"

/* This function is used by the YY_INPUT macro in lexer.l to read 1 byte of input per invocation.
 * Returns 0 if end of input stream.
 * Returns 1 otherwise. In this case, buf[0] holds the next input byte.
 */
int get_input(char *buf, int size) {
	UNUSED(size);
	for (;;) {
		if (EOF_NONE != eof_hit)
			return 0; /* 0 indicates end of input stream */
		if ('\0' == input_buffer_combined[cur_input_index]) {
			/* Input that has been read till now has already been consumed by YY_INPUT.
			 * Read new input line.
			 */
			if (0 == cur_input_more()) {
				return 0; /* No more input. Return 0 to indicate end of input stream */
			}
			/* Done reading a new line (could be an empty line). Redo checks in for loop. */
			continue;
		}
		break;
	}
	assert(1 <= size); /* assert that we have buffer space for at least 1 byte */
	buf[0] = input_buffer_combined[cur_input_index++];
	return 1;
}
