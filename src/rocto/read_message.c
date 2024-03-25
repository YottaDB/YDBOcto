/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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

#include "rocto.h"
#include "message_formats.h"

BaseMessage *read_message(RoctoSession *session, char **buffer, int32_t *buffer_size, int32_t *rocto_err) {
	BaseMessage *message;
	char	    *tmp;
	int32_t	     bytes_to_read, tmp_len;

	// Expand buffer if inadequate to hold initial bytes of a PostgreSQL protocol message
	if ((int)(sizeof(char) + sizeof(uint32_t)) > *buffer_size) {
		free(*buffer);
		*buffer_size = sizeof(char) + sizeof(uint32_t);
		*buffer = (char *)malloc(sizeof(char) * (*buffer_size));
	}
	// Read BaseMessage fields (type, length)
	message = (void *)*buffer;
	*rocto_err = read_bytes(session, (char **)&message, buffer_size, sizeof(char) + sizeof(uint32_t), FALSE);
	if (0 != *rocto_err)
		return NULL;

	// Ensure there is enough space to read the rest of the message by resizing the buffer if necessary
	bytes_to_read = ntohl(message->length) - sizeof(uint32_t);
	/* Include sizeof(char) for the type, since this is read in the above read_bytes call even though it doesn't count toward
	 * the message length.
	 */
	if (bytes_to_read > (*buffer_size - (int32_t)(sizeof(char) + sizeof(uint32_t)))) { // Cast to avoid signedness warning
		*buffer_size = ntohl(message->length) + sizeof(char) + 1;		   // Message type char and null terminator
		tmp = malloc(*buffer_size);
		memcpy(tmp, *buffer, sizeof(char) + sizeof(uint32_t)); // Copy portion of message read above
		free(*buffer);
		*buffer = tmp;
		message = (void *)*buffer;
	}
	/* Create temporary pointer and length variable to satisfy read_bytes interface
	 * This is necessary to allow the passing of a pointer into the middle of the BaseMessage struct rather than its beginning
	 */
	tmp_len = *buffer_size - (sizeof(char) + sizeof(uint32_t));
	tmp = (char *)(&message->data);
	assert(tmp_len >= bytes_to_read);
	// Read the rest of the message
	*rocto_err = read_bytes(session, &tmp, &tmp_len, bytes_to_read, FALSE);
	if (0 != *rocto_err)
		return NULL;

	return message;
}
