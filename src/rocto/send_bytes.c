/* Copyright (C) 2018-2019 YottaDB, LLC
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
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
	int result = 0, tls_errno = 0;
	const char *err_str = NULL;

	if (session->ssl_active) {
#if YDB_TLS_AVAILABLE
		result = gtm_tls_send(session->tls_socket, message, length);
		if (result <= 0 ) {
			if (-1 == result) {
				tls_errno = gtm_tls_errno();
				if(tls_errno == ECONNRESET) {
					return 1;
				}
				else if (-1 == tls_errno) {
					err_str = gtm_tls_get_error();
					FATAL(ERR_ROCTO_TLS_WRITE_FAILED, err_str);
				}
				else {
					FATAL(ERR_SYSCALL, "unknown", tls_errno, strerror(tls_errno));
				}
			}
			return 1;
		}
#endif
	} else {
		result = send(session->connection_fd, message, length, 0);
		if(result < 0) {
			if(errno == ECONNRESET)
				return 1;
			FATAL(ERR_SYSCALL, "send", errno, strerror(errno));
			return 1;
		}
	}
	return 0;
}
