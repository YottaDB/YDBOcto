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
	BaseMessage	*message = NULL;
	Bind		*bind;
	Describe	*describe;
	Execute		*execute;
	Parse		*parse;
	Query		*query;
	ReadyForQuery	*ready_for_query;
	fd_set		rfds;
	struct timeval	select_timeout;
	char		buffer[MAX_STR_CONST];
	int		result;
	boolean_t	send_ready_for_query = TRUE;
	boolean_t	extended_query_error = FALSE;

	TRACE(ERR_ENTERING_FUNCTION, "rocto_main_loop");
	// Send an initial ready
	//ready_for_query = make_ready_for_query(PSQL_TransactionStatus_IDLE);
	//result = send_message(session, (BaseMessage*)(&ready_for_query->type));
	//if(result)
	//	return 0;
	//free(ready_for_query);
	memset(&select_timeout, 0, sizeof(struct timeval));
	select_timeout.tv_usec = 1;

	while (TRUE) {
		// Send ready if there is not a pending message
		do {
			FD_ZERO(&rfds);
			FD_SET(session->connection_fd, &rfds);
			result = select(session->connection_fd+1, &rfds, NULL, NULL, &select_timeout);
		} while(-1 == result && EINTR == errno);

		if (-1 == result) {
			ERROR(ERR_SYSCALL, "select", errno, strerror(errno));
			break;
		}
		if ((0 == result) && send_ready_for_query) {
			ready_for_query = make_ready_for_query(PSQL_TransactionStatus_IDLE);
			result = send_message(session, (BaseMessage*)(&ready_for_query->type));
			if(result)
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
			if(NULL == query)
				break;
			result = handle_query(query, session);
			free(query);
			if(0 != result) {
				break;
			}
			break;
		// Begin Extended Query message types
		case PSQL_Parse:
			send_ready_for_query = FALSE;
			parse = read_parse(message);
			if (NULL == parse)
				break;
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
		// case PSQL_DataRow: // Same letter, different meaning
			describe = read_describe(message);
			if (NULL == describe) {
				extended_query_error = TRUE;
				break;
			}
			result = handle_describe(describe, session);
			free(describe);
			if (1 == result ) {
				extended_query_error = TRUE;
				break;
			}
			break;
		case PSQL_Execute:
		//case PSQL_ErrorResponse: // Same letter code, different meaning
			execute = read_execute(message);
			if (NULL == execute) {
				extended_query_error = TRUE;
				break;
			}
			result = handle_execute(execute, session);
			free(execute);
			if (1 == result) {
				extended_query_error = TRUE;
				break;
			}
			break;
		case PSQL_Sync:
			// After a Sync we can send ReadyForQuery again, as the extended query exchange is complete
			// No read is necessary, as Sync messages only contain a type field, which has already been read
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
			// Gracefully terminate the connection
			shutdown(session->connection_fd, SHUT_RDWR);
			close(session->connection_fd);
			LOG_LOCAL_ONLY(INFO, ERR_ROCTO_CLEAN_DISCONNECT, "");
			return 0;
			break;
		default:
			TRACE(ERR_UNKNOWN_MESSAGE_TYPE, message->type);
			break;
		};
	}
	// Disable sending of messages while we shutdown
	rocto_session.sending_message = TRUE;
	return 0;
}
