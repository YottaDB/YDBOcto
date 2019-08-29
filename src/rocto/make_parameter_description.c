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

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "message_formats.h"


ParameterDescription *make_parameter_description() {
	ParameterDescription *ret;

	ret = (ParameterDescription*)malloc(sizeof(ParameterDescription));
	memset(ret, 0, sizeof(ParameterDescription));

	ret->type = PSQL_ParameterDescription;
	ret->length = htonl(sizeof(uint32_t) + sizeof(int16_t));
	/// TODO: we should return a value other than 0 here
	ret->num_parms = 0;

	return ret;
}
