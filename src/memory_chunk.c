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

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <errno.h>

#include "config.h"
#include "memory_chunk.h"
#include "octo.h"

MemoryChunk *alloc_chunk(size_t size) {
	MemoryChunk *ret;
	void *	     v;

	ret = (MemoryChunk *)malloc(sizeof(MemoryChunk));
	memset(ret, 0, sizeof(MemoryChunk));
	dqinit(ret);
	v = calloc(size, 1);
	ret->value = v;
	ret->max_size = size;
	return ret;
}

void *octo_cmalloc(MemoryChunk *root, size_t size) {
	MemoryChunk *cur, *new;
	void *	     ret;
	size_t	     alloc_size;

	assert(NULL != root);
	cur = root->prev;

	if (cur->offset + size > cur->max_size) {
		alloc_size = ((MEMORY_CHUNK_SIZE < size) ? size : MEMORY_CHUNK_SIZE);
		new = alloc_chunk(alloc_size);
		dqappend(root, new);
		assert(root->prev == new && new->next == root);
		cur = root->prev;
	}
	ret = &cur->value[cur->offset];
	cur->offset += size;
	return ret;
}

void octo_cfree(MemoryChunk *root) {
	MemoryChunk *cur;

	assert(NULL != root);
	cur = root;
	do {
		MemoryChunk *next = cur->next;
		if (next == root) {
			break;
		}
		free(cur->value);
		free(cur);
		cur = next;
	} while (TRUE);
	// Free the root
	free(cur->value);
	free(cur);
}
