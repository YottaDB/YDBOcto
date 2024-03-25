/****************************************************************
 *								*
 * Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include "octo.h"

#ifdef IS_ROCTO
#include "rocto/rocto.h"
#endif

#ifndef ROCTO_COMMON
#include "rocto_common.h"
#endif

/* ---------------- BEGIN : ALL Global variables in Octo ------------------ */

OctoConfig  *config;
MemoryChunk *memory_chunks;
uint64_t     hash_canonical_query_cycle;     // incremented before every outermost call to "hash_canonical_query"
uint64_t     qualify_extract_function_cycle; // incremented before every outermost call to "qualify_extract_function"
int	     cur_input_index;		     // Current index of input_buffer_combined the parser should read from,
					     // and readlines should write to. Effectively marks the end of the
					     // current query.
int old_input_index;			     // The previous value of cur_input_index before the parser modifies it.
					     // Effectively marks the start of the current query.
int   old_input_line_num;		     // The line number pointed to by old_input_index
int   prev_input_line_num;		     // The line number pointed to by the previous value of old_input_index
char *old_input_line_begin;		     // Pointer to the beginning of the line pointed to by old_input_index
int   leading_spaces;			     // leading spaces in the current query it needs to be stored somewhere
					     // accessible but should be ignored, except by the lexer and yyerror
int   cur_input_max;
int   eof_hit;
FILE *inputFile;
char *input_buffer_combined; // The input buffer for octo. Contains the query strings.
int (*cur_input_more)(void);
ydb_buffer_t lex_buffer;	 // String buffer for use in lexer.l
int	     ydb_release_number; /* e.g. the integer 130 in case of r1.30 etc. */

RoctoSession rocto_session;

/* ---------------- END   : ALL Global variables in Octo ------------------ */
