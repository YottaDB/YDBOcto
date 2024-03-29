/****************************************************************
 *								*
 * Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <assert.h>

#include "octo.h"

/* Given a keyword type enum as input, this function returns the keyword name as a string */
char *get_keyword_name(OptionalKeyword keyword) {
	char *ret;

	switch (keyword) {
	case NO_KEYWORD:
		ret = "NO_KEYWORD";
		break;
	case OPTIONAL_GENERATED_ALWAYS_IDENTITY:
		ret = "OPTIONAL_GENERATED_ALWAYS_IDENTITY";
		break;
	case OPTIONAL_GENERATED_BY_DEFAULT_IDENTITY:
		ret = "OPTIONAL_GENERATED_BY_DEFAULT_IDENTITY";
		break;
	case OPTIONAL_DEFAULT:
		ret = "DEFAULT";
		break;
	case PRIMARY_KEY:
		ret = "PRIMARY_KEY";
		break;
	case NOT_NULL:
		ret = "NOT_NULL";
		break;
	case UNIQUE_CONSTRAINT:
		ret = "UNIQUE_CONSTRAINT";
		break;
	case OPTIONAL_GLOBAL:
		ret = "SOURCE";
		break;
	case OPTIONAL_END:
		ret = "END";
		break;
	case OPTIONAL_START:
		ret = "START";
		break;
	case OPTIONAL_DELIM:
		ret = "DELIM";
		break;
	case OPTIONAL_EXTRACT:
		ret = "EXTRACT";
		break;
	case OPTIONAL_CASCADE:
		ret = "CASCADE";
		break;
	case OPTIONAL_RESTRICT:
		ret = "RESTRICT";
		break;
	case OPTIONAL_PIECE:
		ret = "PIECE";
		break;
	case OPTIONAL_KEY_NUM:
		ret = "KEY_NUM";
		break;
	case OPTIONAL_ADVANCE:
		ret = "ADVANCE";
		break;
	case OPTIONAL_LIMIT:
		ret = "LIMIT";
		break;
	case OPTIONAL_DISTINCT:
		ret = "DISTINCT";
		break;
	case OPTIONAL_XREF_INDEX:
		ret = "XREF_INDEX";
		break;
	case OPTIONAL_BOOLEAN_EXPANSION:
		ret = "BOOLEAN_EXPANSION";
		break;
	case OPTIONAL_ASC:
		ret = "ASC";
		break;
	case OPTIONAL_DESC:
		ret = "DESC";
		break;
	case OPTIONAL_STARTINCLUDE:
		ret = "STARTINCLUDE";
		break;
	case OPTIONAL_READONLY:
		ret = "READONLY";
		break;
	case OPTIONAL_READWRITE:
		ret = "READWRITE";
		break;
	case OPTIONAL_ENDPOINT:
		ret = "ENDPOINT";
		break;
	case OPTIONAL_KEEPDATA:
		ret = "KEEPDATA";
		break;
	case OPTIONAL_CHECK_CONSTRAINT:
		ret = "CHECK_CONSTRAINT";
		break;
	case OPTIONAL_AIM_TYPE:
		ret = "AIMTYPE";
		break;
	default:
		assert(FALSE);
		ret = NULL;
		break;
	}
	return ret;
}
