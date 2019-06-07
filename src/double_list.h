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

#ifndef DOUBLE_LIST_H
#define DOUBLE_LIST_H

typedef void *yyscan_t;

// Sets a DQ struct to point to itself
#define dqinit(object) (object)->next = object, (object)->prev = object
// Defines the elements for a DQ struct
#define dqcreate(struct_type) struct struct_type *next, *prev
// Inserts an element behind this one in the doubly linked list
#define dqinsert(self, new_elem, temp)		\
	(temp) = (self)->prev;			\
	(self)->prev->next = (new_elem);	\
	(self)->prev = (new_elem)->prev;	\
	(new_elem)->prev->next = (self);	\
	(new_elem)->prev = (temp);

#endif
