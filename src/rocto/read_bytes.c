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

#include <stdio.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

// Returns:
//	0 for success, -1 for errors other than ECONNRESET, -2 for ECONNRESET
int read_bytes(RoctoSession *session, char **buffer, int32_t *buffer_size, int32_t bytes_to_read, boolean_t allow_resize) {
#if YDB_TLS_AVAILABLE
	int32_t	    tls_errno = 0;
	const char *err_str = NULL;
#endif
	int32_t read_so_far = 0, read_now = 0;

	if (0 > bytes_to_read) {
		ERROR(ERR_INVALID_READ_SIZE, bytes_to_read);
		return -1;
	}
	if ((allow_resize) && (bytes_to_read > *buffer_size)) {
		free(*buffer);
		*buffer_size = bytes_to_read;
		*buffer = (char *)malloc(sizeof(char) * (*buffer_size));
	} else {
		assert(bytes_to_read <= *buffer_size);
	}

	// YDB_TLS_AVAILABLE and ssl_active must both have the same boolean value. If not, an error will be issued at build time.
	if (session->ssl_active) {
#if YDB_TLS_AVAILABLE
		while (read_so_far < bytes_to_read) {
			read_now = gtm_tls_recv(session->tls_socket, &((*buffer)[read_so_far]), bytes_to_read - read_so_far);
			if (GTMTLS_WANT_READ == read_now) {
				continue;
			}
			if (read_now < 0) {
				tls_errno = gtm_tls_errno();
				err_str = GTM_TLS_GET_ERROR(session->tls_socket);
				if (EINTR == tls_errno) {
					ydb_eintr_handler(); /* Needed to invoke YDB signal handler (for signal that caused
							      * EINTR) in a deferred but timely fashion.
							      */
					continue;
				} else if (tls_errno == ECONNRESET) {
					errno = ECONNRESET;
					INFO(INFO_ROCTO_CLEAN_DISCONNECT, "");
					return -2;
				} else {
					ERROR(ERR_ROCTO_TLS_READ_FAILED, err_str);
					return -1;
				}
			}
			read_so_far += read_now;
		}
#endif
	} else {
		while (read_so_far < bytes_to_read) {
			read_now = recv(session->connection_fd, &((*buffer)[read_so_far]), bytes_to_read - read_so_far, 0);
			if (read_now < 0) {
				if (EINTR == errno) {
					ydb_eintr_handler(); /* Needed to invoke YDB signal handler (for signal that caused
							      * EINTR) in a deferred but timely fashion.
							      */
					continue;
				}
				ERROR(ERR_SYSCALL, "read", errno, strerror(errno));
				return -1;
			} else if (read_now == 0) {
				// This means the socket was cleanly closed
				INFO(INFO_ROCTO_CLEAN_DISCONNECT, "");
				return -2;
			}
			read_so_far += read_now;
		}
	}

	return 0;
}
