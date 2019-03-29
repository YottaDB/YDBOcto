
/* Copyright (C) 2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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
	int result, alloc_size;
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
	MemoryChunk *cur, *start;

	assert(root != NULL);
	cur = root;
	do {
		cur = cur->next;
		free(cur->prev->value);
		free(cur->prev);
	} while(cur != root);
}
