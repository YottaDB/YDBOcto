/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
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

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

int send_bytes(RoctoSession *session, char *message, size_t length) {
#if YDB_TLS_AVAILABLE
	int32_t	    result = 0, tls_errno = 0;
	const char *err_str = NULL;
#endif
	int32_t written_so_far = 0, written_now = 0, to_write = length;

	if (session->ssl_active) {
#if YDB_TLS_AVAILABLE
		result = gtm_tls_send(session->tls_socket, message, length);
		if (result <= 0) {
			if (-1 == result) {
				tls_errno = gtm_tls_errno();
				if (ECONNRESET == tls_errno || EPIPE == tls_errno) {
					return 1;
				} else if (-1 == tls_errno) {
					err_str = gtm_tls_get_error();
					ERROR(ERR_ROCTO_TLS_WRITE_FAILED, err_str);
				} else {
					ERROR(ERR_SYSCALL, "ydb_tls_send()", tls_errno, strerror(tls_errno));
				}
			}
			return 1;
		}
#endif
	} else {
		while (written_so_far < to_write) {
			written_now
			    = send(session->connection_fd, &((char *)message)[written_so_far], to_write - written_so_far, 0);
			if (written_now < 0) {
				if (EINTR == errno) {
					ydb_eintr_handler(); /* Needed to invoke YDB signal handler (for signal that caused
							      * EINTR) in a deferred but timely fashion.
							      */
					continue;
				}
				if (errno == ECONNRESET)
					return 1;
				if (errno == EPIPE)
					return 1;
				ERROR(ERR_SYSCALL, "send", errno, strerror(errno));
				return 1;
			}
			written_so_far += written_now;
		}
	}
	return 0;
}
