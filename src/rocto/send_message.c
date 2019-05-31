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

int send_message(RoctoSession *session, BaseMessage *message) {
	int result;

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
			FATAL(ERR_SYSCALL, "send", errno, strerror(errno));
			return 1;
		}
		written_so_far += written_now;
	}
	return 0;
}
