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
	// Align to a page boundary
#if MEMORY_CHUNK_PROTECT != 0
	alloc_size = ((alloc_size + config->page_size) / config->page_size) * config->page_size;
	v = valloc(alloc_size);
#else
	v = malloc(alloc_size);
#endif
	memset(v, 0, alloc_size);
	ret->value = v;
	ret->max_size = alloc_size;
	return ret;
}

void *octo_cmalloc(MemoryChunk *root, size_t size) {
	MemoryChunk *cur, *new, *t;
	void *ret;
	int alloc_size;
	assert(root != NULL);
	cur = root->prev;

	// Make sure size is at least one page, so that we can memprotect
	// the area after
#if MEMORY_CHUNK_PROTECT != 0
	alloc_size = ((size + config->page_size) / config->page_size) * config->page_size + config->page_size;
#else
	alloc_size = size;
#endif

	if(cur->offset + alloc_size > cur->max_size) {
		new = alloc_chunk(alloc_size);
		dqinsert(root, new, t);
		assert(root->prev == new);
		cur = root->prev;
	}
#if MEMORY_CHUNK_PROTECT == 2
	// Put the allocation so that it's snug with the tail to the mprotect'd area, as buffer overflows
	// are more common than underflows
	ret = &cur->value[cur->offset] - (size + config->page_size);
#else
	ret = &cur->value[cur->offset];
#endif
	cur->offset += alloc_size;
#if MEMORY_CHUNK_PROTECT != 0
	// mprotect the area right after to detect buffer overwrites
	assert(cur->offset % config->page_size == 0);
	result = mprotect(&cur->value[cur->offset] - config->page_size, config->page_size, PROT_NONE);
	if(result == -1) {
		printf("Error! %d\n", errno);
	}
#endif
	return ret;
}

void octo_cfree(MemoryChunk *root) {
	MemoryChunk *cur;

	assert(root != NULL);
	cur = root;
	do {
		cur = cur->next;
		free(cur->prev->value);
		free(cur->prev);
	} while(cur != root);
}
