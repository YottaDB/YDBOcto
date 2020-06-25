/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
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

#include <libyottadb.h>

#include "octo.h"

void print_result_row(ydb_buffer_t *row) {
	int	       i, hdr_len, data_len;
	unsigned char *buff_start, *buff, *buff_top, *tail;

	buff = (unsigned char *)row->buf_addr;
	buff_top = buff + row->len_used;
	assert(buff_top > buff);
	assert(row->len_used <= row->len_alloc);
	/* `buff` is a concatenated sequence of <len,str> pairs. But we want only <str> for printing out.
	 * Below logic extracts out the `len` (which could be 1-byte, 2-bytes or 3-bytes long).
	 */
	for (i = 0;; i++) {
		hdr_len = get_mval_len(buff, &data_len);
		if (0 == i) {
			buff_start = buff + hdr_len;
			data_len += hdr_len;
			tail = buff;
		} else {
			assert(0 < hdr_len);
			/* Replace last byte in header with '|' (column/piece separator) and move if needed */
			buff += hdr_len - 1;
			*buff = '|';
			data_len++; /* to include '|' column/piece separator */
			if (tail != buff) {
				memmove(tail, buff, data_len);
			}
		}
		tail += data_len;
		buff += data_len;
		if (buff_top <= buff) {
			assert(buff_top == buff);
			break;
		}
	}
	assert(tail <= buff_top); /* caller `print_temporary_table()` ensures there is space for a '\0' terminator */
	*tail = '\0';
	fprintf(stdout, "%s\n", buff_start);
}
