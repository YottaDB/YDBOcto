/* Copyright (C) 2019 YottaDB, LLC
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
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <time.h>

#include "octo.h"
#include "rocto/rocto.h"
#include "rocto/message_formats.h"
#include "helpers.h"

/**
 * rocto_helper spins and does cleanup, checks for dead children, responds to cancel
 * requests if they arrive, etc. etc.
 */
void *rocto_helper_waitpid(void *args) {
	RoctoSession *session = (RoctoSession*)args;
	while(!session->session_ending) {
		// Wait for PID's
		int wstatus = 0;
		wait(&wstatus);
	}
	return NULL;
}
