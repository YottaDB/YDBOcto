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
#include <assert.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

int rocto_main_loop(RoctoSession *session) {
	BaseMessage *  message = NULL;
	Bind *	       bind;
	Describe *     describe;
	Execute *      execute;
	Parse *	       parse;
	Query *	       query;
	ReadyForQuery *ready_for_query;
	fd_set	       rfds;
	struct timeval select_timeout;
	char	       buffer[MAX_STR_CONST];
	int32_t	       result;
	ydb_long_t     cursorId = -1; // Initialize cursorId to -1 to signal there is no cursor in reuse
	boolean_t      send_ready_for_query = TRUE;
	boolean_t      extended_query_error = FALSE;
	boolean_t      terminated = FALSE;

	TRACE(ERR_ENTERING_FUNCTION, "rocto_main_loop");
	// Send an initial ready
	// ready_for_query = make_ready_for_query(PSQL_TransactionStatus_IDLE);
	// result = send_message(session, (BaseMessage*)(&ready_for_query->type));
	// if(result)
	//	return 0;
	// free(ready_for_query);
	memset(&select_timeout, 0, sizeof(struct timeval));
	select_timeout.tv_usec = 1;

	while (!terminated) {
		// Send ready if there is not a pending message
		do {
			FD_ZERO(&rfds);
			FD_SET(session->connection_fd, &rfds);
			result = select(session->connection_fd + 1, &rfds, NULL, NULL, &select_timeout);
		} while (-1 == result && EINTR == errno);

		if (-1 == result) {
			ERROR(ERR_SYSCALL, "select", errno, strerror(errno));
			break;
		}
		if ((0 == result) && send_ready_for_query) {
			ready_for_query = make_ready_for_query(PSQL_TransactionStatus_IDLE);
			result = send_message(session, (BaseMessage *)(&ready_for_query->type));
			if (result)
				break;
			free(ready_for_query);
		}
		int32_t rocto_err = 0;
		message = read_message(session, buffer, MAX_STR_CONST, &rocto_err);
		if (NULL == message) {
			break;
		}
		TRACE(ERR_READ_MESSAGE, message->type, ntohl(message->length));
		// Discard any messages received after an error in an extended query exchange until hitting a Sync message
		if (extended_query_error && (PSQL_Sync != message->type))
			continue;
		switch (message->type) {
		case PSQL_Query:
			query = read_query(message);
			if (NULL == query)
				break;
			result = handle_query(query, session);
			free(query);
			if (0 != result) {
				break;
			}
			break;
		// Begin Extended Query message types
		case PSQL_Parse:
			send_ready_for_query = FALSE;
			parse = read_parse(message);
			if (NULL == parse) {
				extended_query_error = TRUE;
				break;
			}
			result = handle_parse(parse, session);
			free(parse);
			if (1 == result) {
				extended_query_error = TRUE;
				break;
			}
			break;
		case PSQL_Bind:
			bind = read_bind(message);
			if (NULL == bind) {
				extended_query_error = TRUE;
				break;
			}
			result = handle_bind(bind, session);
			free(bind->parms);
			free(bind);
			if (1 == result) {
				extended_query_error = TRUE;
				break;
			}
			break;
		case PSQL_Describe:
			describe = read_describe(message);
			if (NULL == describe) {
				extended_query_error = TRUE;
				break;
			}
			result = handle_describe(describe, session);
			free(describe);
			if (1 == result) {
				extended_query_error = TRUE;
				break;
			}
			break;
		case PSQL_Execute:
			execute = read_execute(message);
			if (NULL == execute) {
				extended_query_error = TRUE;
				break;
			}
			// Pass cursorId as a pointer so we can reuse the same cursor for
			// subsequent Execute requests in the PortalSuspended case, when
			// result rows remain to be sent.
			result = handle_execute(execute, session, &cursorId);
			free(execute);
			if (1 == result) { // Catch failure from send_message, if any
				extended_query_error = TRUE;
				break;
			}
			break;
		case PSQL_Sync:
			// After a Sync we can send ReadyForQuery again, as the extended query exchange is complete
			// No read is necessary, as Sync messages only contain a type field, which has already been read
			// The cursor should also be reset now, as Sync concludes an Extended Query sequence.
			// Any unsent rows on the cursor are thus discarded.
			cursorId = -1;
			send_ready_for_query = TRUE;
			extended_query_error = FALSE;
			break;
		// End Extended Query message types
		case PSQL_Flush:
			// Do nothing for now, until extended query functionality added, see issue #375
			// TODO: Add error handling when this feature is supported
			// flush = read_flush(message);
			break;
		case PSQL_Terminate:
			LOG_LOCAL_ONLY(INFO, ERR_ROCTO_CLEAN_DISCONNECT, "");
			terminated = TRUE;
			break;
		default:
			TRACE(ERR_UNKNOWN_MESSAGE_TYPE, message->type);
			break;
		};
	}
	// Gracefully terminate the connection
	if (session->ssl_active) {
#if YDB_TLS_AVAILABLE
		gtm_tls_session_close(&session->tls_socket);
#else
		assert(FALSE);
#endif
	} else {
		shutdown(session->connection_fd, SHUT_RDWR);
		close(session->connection_fd);
	}
	// Disable sending of messages while we shutdown
	rocto_session.sending_message = TRUE;
	cleanup_tables();
	return 0;
}
