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

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <errno.h>

#include "config.h"
#include "memory_chunk.h"

MemoryChunk *alloc_chunk(size_t size) {
	MemoryChunk *ret;
	size_t alloc_size;
	void *v;

	ret = (MemoryChunk*)malloc(sizeof(MemoryChunk));
	memset(ret, 0, sizeof(MemoryChunk));
	dqinit(ret);

	alloc_size = MEMORY_CHUNK_SIZE;
	if(size > MEMORY_CHUNK_SIZE) {
		alloc_size = size;
	}
	v = calloc(alloc_size, 1);
	ret->value = v;
	ret->max_size = alloc_size;
	return ret;
}

void *octo_cmalloc(MemoryChunk *root, size_t size) {
	MemoryChunk *cur, *new, *t;
	void *ret;
	assert(root != NULL);
	cur = root->prev;

	if(cur->offset + size > cur->max_size) {
		new = alloc_chunk(size);
		dqinsert(root, new, t);
		assert(root->prev == new && new->next == root);
		cur = root->prev;
	}
	ret = &cur->value[cur->offset];
	cur->offset += size;
	return ret;
}

void octo_cfree(MemoryChunk *root) {
	MemoryChunk *cur;

	assert(root != NULL);
	cur = root;
	do {
		MemoryChunk *next = cur->next;
		if(next == root) {
			break;
		}
		free(cur->value);
		free(cur);
		cur = next;
	} while(TRUE);
	// Free the root
	free(cur->value);
	free(cur);
}
