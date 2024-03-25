/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#ifndef MEMORY_CHUNK_H
#define MEMORY_CHUNK_H

#include <sys/types.h>
#include <unistd.h>

#include "constants.h"
#include "double_list.h"

#define OCTO_CFREE(MEMORY_CHUNKS)                  \
	{                                          \
		if (NULL != MEMORY_CHUNKS) {       \
			octo_cfree(MEMORY_CHUNKS); \
			MEMORY_CHUNKS = NULL;      \
		}                                  \
	}

typedef struct MemoryChunk {
	dqcreate(MemoryChunk);
	size_t offset, max_size;
	char  *value;
} MemoryChunk;

MemoryChunk *alloc_chunk(size_t size);
void	    *octo_cmalloc(MemoryChunk *root, size_t size);
void	     octo_cfree(MemoryChunk *root);

// GLOBAL
extern MemoryChunk *memory_chunks;

#endif
