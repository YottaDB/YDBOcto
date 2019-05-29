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
#include <errno.h>
#include <assert.h>

#include <openssl/ssl.h>
#include <openssl/err.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

int send_message(RoctoSession *session, BaseMessage *message) {
	int result = 0, ossl_error = 0;
	unsigned long ossl_error_code = 0;
	char *err = NULL;

	TRACE(ERR_ENTERING_FUNCTION, "send_message");

	// +1 for the message format flag
	INFO(ERR_SEND_MESSAGE, message->type, ntohl(message->length));
	int written_so_far = 0, written_now = 0, to_write = ntohl(message->length) + 1;

	while(written_so_far < to_write) {
		written_now = send(session->connection_fd, &((char*)message)[written_so_far],
				to_write - written_so_far, 0);
		if(written_now < 0) {
			if(errno == EINTR)
				continue;
			if(errno == ECONNRESET)
				return 1;
			if(errno == EPIPE)
				return 1;
			FATAL(ERR_SYSCALL, "send", errno, strerror(errno));
			return 1;
		}
		written_so_far += written_now;
	}
	return 0;
}
