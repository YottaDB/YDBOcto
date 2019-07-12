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

int read_bytes(RoctoSession *session, char *buffer, int buffer_size, int bytes_to_read) {
#if YDB_TLS_AVAILABLE
	int tls_errno = 0;
	const char *err_str = NULL;
#endif
	int read_so_far = 0, read_now = 0;


	if(bytes_to_read > buffer_size) {
		WARNING(ERR_READ_TOO_LARGE, bytes_to_read, buffer_size);
		return -1;
	} else if(bytes_to_read < 0) {
		WARNING(ERR_INVALID_READ_SIZE, bytes_to_read);
		return -1;
	}

	// YDB_TLS_AVAILABLE and ssl_active must both have the same boolean value. If not, an error will be issued at build time.
	if (session->ssl_active) {
#if YDB_TLS_AVAILABLE
		while(read_so_far < bytes_to_read) {
			read_now = gtm_tls_recv(session->tls_socket, &buffer[read_so_far],
					bytes_to_read - read_so_far);
			if (GTMTLS_WANT_READ == read_now) {
					continue;
			}
			if (read_now < 0) {
				tls_errno = gtm_tls_errno();
				err_str = gtm_tls_get_error();
				if (tls_errno == EINTR) {
					continue;
				} else if (tls_errno == ECONNRESET) {
					errno = ECONNRESET;
					INFO(ERR_ROCTO_CLEAN_DISCONNECT, "");
					return -2;
				} else {
					FATAL(ERR_ROCTO_TLS_READ_FAILED, err_str);
				}
			}
			read_so_far += read_now;
		}
#endif
	} else {
		while(read_so_far < bytes_to_read) {
			read_now = recv(session->connection_fd, &buffer[read_so_far],
					bytes_to_read - read_so_far, 0);
			if(read_now < 0) {
				if(errno == EINTR)
					continue;
				WARNING(ERR_SYSCALL, "read", errno, strerror(errno));
				return -1;
			} else if(read_now == 0) {
				// This means the socket was cleanly closed
				INFO(ERR_ROCTO_CLEAN_DISCONNECT, "");
				return -2;
			}
			read_so_far += read_now;
		}
	}

	return 0;
}
