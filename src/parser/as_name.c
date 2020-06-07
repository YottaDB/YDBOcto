/****************************************************************
 *								*
 * Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>
#include <ctype.h>	/* needed for "toupper" */

#include "octo.h"
#include "octo_types.h"

// Function invoked by the rule named "as_name" in src/parser/select.y
int as_name(SqlStatement *as_name, ParseContext *parse_context)
{
	char	*c;
	int	status;

	/* SqlValue type of "as_name" is set to "STRING_LITERAL" in order to prevent multiple plan generation
	 * for queries differing only by alias name or LITERAL value.
	 */
	assert(value_STATEMENT == as_name->type);
	as_name->v.value->type = STRING_LITERAL;
	c = as_name->v.value->v.string_literal;
	while('\0' != *c) {
		*c = toupper(*c);
		c++;
	}
	status = parse_literal_to_parameter(parse_context, as_name->v.value, FALSE);
	return status;
}
