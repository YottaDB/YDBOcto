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

#include <assert.h>

#include "octo.h"

/* Returns the byte length of the `len` header in the mval (`len,str` tuple).
 * Sets `*data_len` to the length of str (not including the header length).
 *
 * See `mvalPiece` entryref in `src/aux/_ydboctoplanhelpers.m` for details.
 * Any changes there most likely need to be reflected here too.
 */
int get_mval_len(unsigned char *buff, int *data_len)
{
	int	byte1, len, hdr_len;

	byte1 = *buff;
	if (0 == byte1) {
		/* $ZYSQLNULL */
		len = 1;
		hdr_len = 1;
	} else if (128 > byte1) {
		/* 1-byte header */
		len = byte1;
		hdr_len = 1;
	} else if (192 > byte1) {
		/* 2-byte header */
		hdr_len = 2;
		len = (byte1 - 128) * 256 + (unsigned int)buff[1];
	} else {
		/* 3-byte header */
		hdr_len = 3;
		len = (byte1 - 192) * 65536 + ((unsigned int)buff[1] * 256) + (unsigned int)buff[2];
	}
	assert(len >= hdr_len);
	*data_len = len - hdr_len;	/* take away header length */
	return hdr_len;
}
