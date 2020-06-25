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
	RoctoSession *session = (RoctoSession *)args;
	while (!session->session_ending) {
		// Wait for PID's
		int   wstatus = 0;
		pid_t result = wait(&wstatus);
		if (result < 0) {
			// If there are no children active, just continue
			if (errno == ECHILD) {
				// Sleep for a brief time if there were no children
				struct timespec sleep = {1, 0};
				nanosleep(&sleep, NULL);
				continue;
			}
			FATAL(ERR_SYSCALL, "wait", errno, strerror(errno));
		}
	}
	return NULL;
}
