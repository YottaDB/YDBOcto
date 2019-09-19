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


#include "octo.h"
#include "octo_types.h"


/* trims duplicate .*'s from regex
 * invoked by LIKE, TILDE rules in src/parser.y
 */
void trim_dot_star(SqlValue *regex){
	int trim = FALSE;
	char *c = regex->v.string_literal, *dest = regex->v.string_literal;
	while('\0' != *c){
		/* peek at next character to see if it is a * */
		if('.' == *c && '*' == *(c+1)){
			/* if we are trimming skip these two characters */
			if(trim){
				c += 2;
			} else {
				/* otherwise start trimming and copy this .* */
				trim = TRUE;
				*(dest++) = *(c++);
				*(dest++) = *(c++);
			}
		} else {
			/* anything else just copy it and reset trimming */
			*(dest++) = *(c++);
			trim = FALSE;
		}
	}
	*dest = '\0';
	return;
}
