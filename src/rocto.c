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
#include <stdlib.h>
#include <netinet/in.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <arpa/inet.h>

#include "octo.h"
#include "rocto/rocto.h"
#include "rocto/message_formats.h"
#include "helpers.h"

int main(int argc, char **argv) {
	BaseMessage *base_message;
	StartupMessage *startup_message;
	ErrorResponse *err;
	AuthenticationMD5Password *md5auth;
	AuthenticationOk *authok;
	ParameterStatus *parameter_status;
	struct sockaddr_in6 addressv6;
	struct sockaddr_in *address;
	int sfd, cfd, opt, addrlen, result, status;
	int cur_parm, i;
	pid_t child_id;
	char buffer[MAX_STR_CONST];
	RoctoSession session;

	ydb_buffer_t ydb_buffers[2];
	ydb_buffer_t *global_buffer = &(ydb_buffers[0]), *session_id_buffer = &(ydb_buffers[1]);
	ydb_buffer_t z_status, z_status_value;

	octo_init(argc, argv, TRUE);
	config->record_error_level = TRACE;
	INFO(CUSTOM_ERROR, "rocto started");

	// Setup the address first so we know which protocol to use
	memset(&addressv6, 0, sizeof(struct sockaddr_in6));
	address = (struct sockaddr_in *)(&addressv6);
	address->sin_family = AF_INET;
	if(inet_pton(AF_INET, config->rocto_config.address, &address->sin_addr) != 1) {
		addressv6.sin6_family = AF_INET6;
		if(inet_pton(AF_INET6, config->rocto_config.address, &addressv6.sin6_addr) != 1) {
			FATAL(ERR_BAD_ADDRESS, config->rocto_config.address);
		}
	}
	address->sin_port = htons(config->rocto_config.port);

	if((sfd = socket(address->sin_family, SOCK_STREAM, 0)) == -1)
	{
		FATAL(ERR_SYSCALL, "socket", errno);
	}

	opt = 0;
	if(setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt)) == -1) {
		FATAL(ERR_SYSCALL, "setsockopt", errno);
	}



	if(bind(sfd, (struct sockaddr *)&addressv6, sizeof(addressv6)) < 0) {
		FATAL(ERR_SYSCALL, "bind", errno);
	}


	while (TRUE) {
		if(listen(sfd, 3) < 0) {
			FATAL(ERR_SYSCALL, "listen", errno);
		}
		if((cfd = accept(sfd, (struct sockaddr *)&address, &addrlen)) < 0) {
			FATAL(ERR_SYSCALL, "accept", errno);
		}
		child_id = fork();
		if(child_id != 0)
			continue;
		INFO(ERR_CLIENT_CONNECTED);
		// First we read the startup message, which has a special format
		// 2x32-bit ints
		session.connection_fd = cfd;
		// Establish the connection first
		session.session_id = NULL;
		read_bytes(&session, buffer, MAX_STR_CONST, sizeof(int) * 2);
		startup_message = read_startup_message(&session, buffer, sizeof(int) * 2, &err);
		if(startup_message == NULL) {
			send_message(&session, (BaseMessage*)(&err->type));
			free(err);
			break;
		}
		// Pretend to require md5 authentication
		md5auth = make_authentication_md5_password();
		result = send_message(&session, (BaseMessage*)(&md5auth->type));
		if(result)
			break;
		free(md5auth);

		// This next message is the user sending the password; ignore it
		base_message = read_message(&session, buffer, MAX_STR_CONST);
		if(base_message == NULL)
			break;

		// Ok
		authok = make_authentication_ok();
		send_message(&session, (BaseMessage*)(&authok->type));
		free(authok);

		// Enter the main loop
		global_buffer = &(ydb_buffers[0]);
		session_id_buffer = &(ydb_buffers[1]);
		YDB_LITERAL_TO_BUFFER("$ZSTATUS", &z_status);
		INIT_YDB_BUFFER(&z_status_value, MAX_STR_CONST);
		YDB_STRING_TO_BUFFER(config->global_names.session, global_buffer);
		INIT_YDB_BUFFER(session_id_buffer, MAX_STR_CONST);
		printf("session length: %d\n", global_buffer->len_used);
		status = ydb_incr_s(global_buffer, 0, NULL, NULL, session_id_buffer);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		session.session_id = session_id_buffer;

		// Set parameters
		for(cur_parm = 0; i < startup_message->num_parameters; i++) {
			set(startup_message->parameters[i].value, config->global_names.session, 3,
					session.session_id->buf_addr, "variables",
					startup_message->parameters[i].name);
			parameter_status = make_parameter_status(&startup_message->parameters[i]);
			result = send_message(&session, (BaseMessage*)(&parameter_status->type));
			free(parameter_status);
			if(result) {
				return 0;
			}
		}
		rocto_main_loop(&session);
		break;
	}


	return 0;
}
