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

#ifndef HELPERS_H
#define HELPERS_H

// Print a YottaDB global/local variable node stored in a ydb_buffer_t array (Debugging only)
#define PRINT_NODE(SUBSARRAY, NUM_SUBS)					\
{									\
	printf("^%s(", SUBSARRAY[0].buf_addr);				\
	int i;								\
	for (i = 1; i <= NUM_SUBS; i++) {				\
		if (NUM_SUBS == i)					\
			printf("\"%s\")\n", SUBSARRAY[i].buf_addr);	\
		else							\
			printf("\"%s\",", SUBSARRAY[i].buf_addr);	\
	}								\
}

ydb_buffer_t *make_buffers(char *global, size_t num_args, ...);
ydb_buffer_t *vmake_buffers(char *global, size_t num_args, va_list args);

#endif
