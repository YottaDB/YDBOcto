/****************************************************************
 *								*
 * Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	*
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
#include <stdio.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "message_formats.h"
#include "rocto.h"

CommandComplete *make_command_complete(SqlStatementType cmd_type, int32_t num_rows) {
	CommandComplete *ret;
	int32_t		 length = 0, command_tag_length = 0;
	char		 command_tag[MAX_TAG_LEN];

	// Include row count for SELECT statements, omit for SET and SHOW
	// If not SELECT, SET, or SHOW omit command tag entirely to prevent tags from erroneously appearing in client output.
	// These cases are based on observed client behavior (psql) that does not seem to be documented, so this may need to be
	// updated if this observed behavior changes.
	switch (cmd_type) {
	case select_STATEMENT:
		snprintf(command_tag, MAX_TAG_LEN, "SELECT %d", num_rows); /* num_rows is number of rows sent */
		break;
	case insert_STATEMENT:
		/* The below format conforms Postgres output (see https://www.postgresql.org/docs/9.5/sql-insert.html).
		 *
		 * ---------------------------------------------------------------------------------
		 * On successful completion, an INSERT command returns a command tag of the form
		 *	INSERT oid count
		 * The count is the number of rows inserted or updated. If count is exactly one, and the target
		 * table has OIDs, then oid is the OID assigned to the inserted row. The single row must have
		 * been inserted rather than updated. Otherwise oid is zero.
		 * ---------------------------------------------------------------------------------
		 *
		 * Note: In rocto, the "oid" and "count" are currently 0. Would be nice to display accurate "count" in the future
		 * hence the TODO note below.
		 *
		 * TODO : YDBOcto#502 : Print accurate "count" below.
		 */
		snprintf(command_tag, MAX_TAG_LEN, "INSERT 0 0");
		break;
	case set_STATEMENT:
		snprintf(command_tag, MAX_TAG_LEN, "SET");
		break;
	case show_STATEMENT:
		snprintf(command_tag, MAX_TAG_LEN, "SHOW");
		break;
	default:
		return NULL;
		break;
	}

	length += sizeof(uint32_t);
	command_tag_length = strlen(command_tag) + 1; // count null
	length += command_tag_length;
	ret = (CommandComplete *)malloc(length + sizeof(CommandComplete) - sizeof(uint32_t));
	memset(ret, 0, length + sizeof(CommandComplete) - sizeof(uint32_t));

	ret->type = PSQL_CommandComplete;
	ret->length = htonl(length);
	memcpy(ret->data, command_tag, command_tag_length);

	return ret;
}
