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
	BaseMessage	*message;
	Bind		*bind;
	Flush		*flush;
	Describe	*describe;
	Execute		*execute;
	Parse		*parse;
	Query		*query;
	ReadyForQuery	*ready_for_query;
	Sync		*sync;
	Terminate	*terminate;
	fd_set		rfds;
	struct timeval	select_timeout;
	char		buffer[MAX_STR_CONST];
	int		result;
	int		send_ready_for_query = TRUE;

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

		if(-1 == result) {
			ERROR(ERR_SYSCALL, "select", errno, strerror(errno));
			break;
		}
		if(0 == result && send_ready_for_query) {
			ready_for_query = make_ready_for_query(PSQL_TransactionStatus_IDLE);
			result = send_message(session, (BaseMessage*)(&ready_for_query->type));
			if(result)
				break;
			free(ready_for_query);
		}
		send_ready_for_query = TRUE;
		int32_t rocto_err = 0;
		message = read_message(session, buffer, MAX_STR_CONST, &rocto_err);
		if(NULL == message) {
			break;
		}
		TRACE(ERR_READ_MESSAGE, message->type, ntohl(message->length));
		switch(message->type) {
		case PSQL_Bind:
			bind = read_bind(message);
			if(NULL == bind) {
				break;
			}
			result = handle_bind(bind, session);
			if(1 == result) {
				return 0;
			}
			break;
		case PSQL_Query:
			query = read_query(message);
			if(NULL == query) {
				break;
			}
			result = handle_query(query, session);
			if(1 == result) {
				return 0;
			}
			break;
		case PSQL_Execute:
		//case PSQL_ErrorResponse: // Same letter code, different meaning
			execute = read_execute(message);
			if(NULL == execute) {
				break;
			}
			result = handle_execute(execute, session);
			if(1 == result) {
				return 0;
			}
			send_ready_for_query = FALSE;
			break;
		case PSQL_Flush:
			// Do nothing for now, until extended query functionality added
			flush = read_flush(message);
			if (NULL == flush) {
				break;
			}
			break;
		case PSQL_Parse:
			parse = read_parse(message);
			if(NULL == parse) {
				break;
			}
			result = handle_parse(parse, session);
			if(1 == result) {
				return 0;
			}
			break;
		case PSQL_Sync:
			// This requires no action right now, but eventually we will have to end transactions
			sync = read_sync(message);
			if (NULL == sync) {
				break;
			}
			break;
		case PSQL_Describe:
		// case PSQL_DataRow: // Same letter, different meaning
			describe = read_describe(message);
			if(NULL == describe) {
				break;
			}
			result = handle_describe(describe, session);
			if(1 == result ) {
				return 0;
			}
			break;
		case PSQL_Terminate:
			// Do nothing for now, until Terminate functionality added
			terminate = read_terminate(message);
			if (NULL == terminate) {
				break;
			}
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
