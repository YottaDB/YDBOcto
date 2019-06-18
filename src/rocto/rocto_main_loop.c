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
	Query *query;
	Bind *bind;
	Execute *execute;
	Parse *parse;
	Describe *describe;
	ErrorResponse *err;
	BaseMessage *message;
	ReadyForQuery *ready_for_query;
	fd_set rfds;
	struct timeval select_timeout;
	int result;
	char buffer[MAX_STR_CONST];
	int send_ready_for_query = TRUE;

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
		} while(result == -1 && errno == EINTR);

		if(result == -1) {
			FATAL(ERR_SYSCALL, "select", errno, strerror(errno));
		}
		if(result == 0 && send_ready_for_query) {
			ready_for_query = make_ready_for_query(PSQL_TransactionStatus_IDLE);
			result = send_message(session, (BaseMessage*)(&ready_for_query->type));
			if(result)
				break;
			free(ready_for_query);
		}
		send_ready_for_query = TRUE;
		message = read_message(session, buffer, MAX_STR_CONST);
		if(message == NULL) {
			break;
		}
		TRACE(ERR_READ_MESSAGE, message->type, ntohl(message->length));
		switch(message->type) {
		case PSQL_Bind:
			bind = read_bind(message, &err);
			if(bind == NULL) {
				send_message(session, (BaseMessage*)(&err->type));
				free_error_response(err);
				break;
			}
			result = handle_bind(bind, session);
			if(result == 1) {
				return 0;
			}
			break;
		case PSQL_BindComplete:
			break;
		case PSQL_ReadyForQuery:
			// We don't expect this, issue an error
			break;
		case PSQL_Query:
			query = read_query(message, &err);
			if(query == NULL) {
				send_message(session, (BaseMessage*)(&err->type));
				free_error_response(err);
				break;
			}
			result = handle_query(query, session);
			if(result == 1) {
				return 0;
			}
			break;
		case PSQL_EmptyQueryResponse:
			// We don't expect this, issue an error
			break;
		case PSQL_RowDescription:
			// We don't expect this, issue an error
			break;
		case PSQL_CommandComplete:
			// We don't expect this, issue an error
			break;
		// All 3 of these have the same message code
		case PSQL_AuthenticationMD5Password:
		//case PSQL_Authenication:
		//case PSQL_AuthenticationOk:
			// We don't expect this, issue an error
			break;
		case PSQL_Execute:
		//case PSQL_ErrorResponse: // Same letter code, different meaning
			execute = read_execute(message, &err);
			if(execute == NULL) {
				send_message(session, (BaseMessage*)(&err->type));
				free_error_response(err);
				break;
			}
			result = handle_execute(execute, session);
			if(result == 1) {
				return 0;
			}
			send_ready_for_query = FALSE;
			break;
		case PSQL_Parse:
			parse = read_parse(message, &err);
			if(parse == NULL) {
				send_message(session, (BaseMessage*)(&err->type));
				free_error_response(err);
				break;
			}
			result = handle_parse(parse, session);
			if(result == 1) {
				return 0;
			}
			break;
		case PSQL_Sync:
			// This requires no action right now, but eventually we will have to end transactions
			break;
		case PSQL_Describe:
		// case PSQL_DataRow: // Same letter, different meaning
			describe = read_describe(message, &err);
			if(describe == NULL) {
				send_message(session, (BaseMessage*)(&err->type));
				free_error_response(err);
				break;
			}
			result = handle_describe(describe, session);
			if(result == 1) {
				return 0;
			}
			break;
		case PSQL_Terminate:
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
