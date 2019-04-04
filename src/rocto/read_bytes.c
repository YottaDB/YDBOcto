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
	int read_so_far = 0, read_now = 0;

	if(bytes_to_read > buffer_size) {
		WARNING(ERR_READ_TOO_LARGE, bytes_to_read, buffer_size);
		return 1;
	} else if(bytes_to_read < 0) {
		WARNING(ERR_INVALID_READ_SIZE, bytes_to_read);
		return 1;
	}

	while(read_so_far < bytes_to_read) {
		read_now = recv(session->connection_fd, &buffer[read_so_far],
				bytes_to_read - read_so_far, 0);
		if(read_now < 0) {
			if(errno == EINTR)
				continue;
			WARNING(ERR_SYSCALL, "read", errno);
			return 1;
		} else if(read_now == 0) {
			// This means the socket was cleanly closed
			return 1;
		}
		read_so_far += read_now;
	};

	return 0;
}
