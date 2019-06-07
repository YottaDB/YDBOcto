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

#ifndef HELPERS_H
#define HELPERS_H

ydb_buffer_t *make_buffers(char *global, size_t num_args, ...);
ydb_buffer_t *vmake_buffers(char *global, size_t num_args, va_list args);
void set(char *new_value, char *global, size_t num_args, ...);
ydb_buffer_t *get(char *global, size_t num_args, ...);

#endif
