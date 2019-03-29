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
