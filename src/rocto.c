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
#include <netdb.h>
#include <pthread.h>
#include <signal.h>

#include "octo.h"
#include "rocto/rocto.h"
#include "rocto/message_formats.h"
#include "helpers.h"

void handle_sigint(int sig) {
	rocto_session.session_ending = TRUE;
}

int main(int argc, char **argv) {
	BaseMessage *base_message = NULL;
	StartupMessage *startup_message = NULL;
	ErrorResponse *err = NULL;
	AuthenticationMD5Password *md5auth = NULL;
	AuthenticationOk *authok = NULL;
	ParameterStatus *parameter_status = NULL;
	struct sockaddr_in6 addressv6;
	struct sockaddr_in *address = NULL;
	int sfd, cfd, opt, addrlen, result, status;
	int cur_parm = 0, i = 0;
	pid_t child_id = 0;
	char buffer[MAX_STR_CONST];
	char host_buf[NI_MAXHOST], serv_buf[NI_MAXSERV];
	StartupMessageParm message_parm;
	sfd = cfd = opt = addrlen = result = status = 0;

	ydb_buffer_t ydb_buffers[2], *var_defaults, *var_sets, var_value;
	ydb_buffer_t *global_buffer = &(ydb_buffers[0]), *session_id_buffer = &(ydb_buffers[1]);
	ydb_buffer_t z_status, z_status_value;

	// Initialize a handler to respond to ctrl + c
	signal(SIGINT, handle_sigint);

	// Initialize connection details in case errors prior to connections
	rocto_session.ip = "IP_UNSET";
	rocto_session.port = "PORT_UNSET";

	octo_init(argc, argv, TRUE);
	INFO(CUSTOM_ERROR, "rocto started");

	// Disable sending log messages until all startup messages have been sent
	rocto_session.sending_message = TRUE;

	// Setup the address first so we know which protocol to use
	memset(&addressv6, 0, sizeof(struct sockaddr_in6));
	address = (struct sockaddr_in *)(&addressv6);
	address->sin_family = AF_INET;
	addrlen = sizeof(struct sockaddr_in6);
	if(inet_pton(AF_INET, config->rocto_config.address, &address->sin_addr) != 1) {
		addressv6.sin6_family = AF_INET6;
		if(inet_pton(AF_INET6, config->rocto_config.address, &addressv6.sin6_addr) != 1) {
			FATAL(ERR_BAD_ADDRESS, config->rocto_config.address);
		}
	}
	address->sin_port = htons(config->rocto_config.port);

	if((sfd = socket(address->sin_family, SOCK_STREAM, 0)) == -1)
	{
		FATAL(ERR_SYSCALL, "socket", errno, strerror(errno));
	}

	opt = 0;
	if(setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt)) == -1) {
		FATAL(ERR_SYSCALL, "setsockopt", errno, strerror(errno));
	}

	if(bind(sfd, (struct sockaddr *)&addressv6, sizeof(addressv6)) < 0) {
		FATAL(ERR_SYSCALL, "bind", errno, strerror(errno));
	}

	// Spin off another thread to keep an eye on dead processes
	pthread_t thread_id;
	pthread_create(&thread_id, NULL, rocto_helper_waitpid, (void*)(&rocto_session));

	while (!rocto_session.session_ending) {
		if(listen(sfd, 3) < 0) {
			if(rocto_session.session_ending) {
				break;
			}
			FATAL(ERR_SYSCALL, "listen", errno, strerror(errno));
		}
		if((cfd = accept(sfd, (struct sockaddr *)&address, &addrlen)) < 0) {
			if(rocto_session.session_ending) {
				break;
			}
			if(errno == EINTR) {
				continue;
			}
			FATAL(ERR_SYSCALL, "accept", errno, strerror(errno));
		}
		child_id = fork();
		if(child_id != 0)
			continue;
		thread_id = 0;
		// First we read the startup message, which has a special format
		// 2x32-bit ints
		rocto_session.connection_fd = cfd;
		if (config->rocto_config.use_dns) {
			result = getnameinfo((const struct sockaddr *)&address, addrlen,
					host_buf, NI_MAXHOST, serv_buf, NI_MAXSERV, 0);
		} else {
			result = getnameinfo((const struct sockaddr *)&address, addrlen,
					host_buf, NI_MAXHOST, serv_buf, NI_MAXSERV, NI_NUMERICHOST | NI_NUMERICSERV );
		}
		if (0 != result) {
			FATAL(ERR_SYSCALL, "getnameinfo", errno, strerror(errno));
		}
		rocto_session.ip = host_buf;
		rocto_session.port = serv_buf;
		INFO(ERR_CLIENT_CONNECTED);
		// Establish the connection first
		rocto_session.session_id = NULL;
		read_bytes(&rocto_session, buffer, MAX_STR_CONST, sizeof(int) * 2);
		startup_message = read_startup_message(&rocto_session, buffer, sizeof(int) * 2, &err);
		if(startup_message == NULL) {
			send_message(&rocto_session, (BaseMessage*)(&err->type));
			free(err);
			break;
		}
		// Pretend to require md5 authentication
		md5auth = make_authentication_md5_password();
		result = send_message(&rocto_session, (BaseMessage*)(&md5auth->type));
		if(result)
			break;
		free(md5auth);

		// This next message is the user sending the password; ignore it
		base_message = read_message(&rocto_session, buffer, MAX_STR_CONST);
		if(base_message == NULL)
			break;

		// Ok
		authok = make_authentication_ok();
		send_message(&rocto_session, (BaseMessage*)(&authok->type));
		free(authok);

		rocto_session.sending_message = FALSE;

		// Enter the main loop
		global_buffer = &(ydb_buffers[0]);
		session_id_buffer = &(ydb_buffers[1]);
		YDB_LITERAL_TO_BUFFER("$ZSTATUS", &z_status);
		INIT_YDB_BUFFER(&z_status_value, MAX_STR_CONST);
		YDB_STRING_TO_BUFFER(config->global_names.session, global_buffer);
		INIT_YDB_BUFFER(session_id_buffer, MAX_STR_CONST);
		status = ydb_incr_s(global_buffer, 0, NULL, NULL, session_id_buffer);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		rocto_session.session_id = session_id_buffer;
		rocto_session.session_id->buf_addr[rocto_session.session_id->len_used] = '\0';

		// Populate default parameters
		var_defaults = make_buffers(config->global_names.octo, 2, "variables", "");
		INIT_YDB_BUFFER(&var_defaults[2], MAX_STR_CONST);
		INIT_YDB_BUFFER(&var_value, MAX_STR_CONST);
		var_sets = make_buffers(config->global_names.session, 3, rocto_session.session_id->buf_addr,
				"variables", "");
		var_sets[3] = var_defaults[2];
		do {
			status = ydb_subscript_next_s(&var_defaults[0], 2, &var_defaults[1], &var_defaults[2]);
			if(status == YDB_ERR_NODEEND)
				break;
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);
			status = ydb_get_s(&var_defaults[0], 2, &var_defaults[1], &var_value);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);
			var_sets[3] = var_defaults[2];
			status = ydb_set_s(&var_sets[0], 3, &var_sets[1], &var_value);
		} while(TRUE);

		// Set parameters
		for(cur_parm = 0; cur_parm < startup_message->num_parameters; cur_parm++) {
			set(startup_message->parameters[cur_parm].value, config->global_names.session, 3,
					rocto_session.session_id->buf_addr, "variables",
					startup_message->parameters[cur_parm].name);
		}

		var_sets[3].len_used = 0;
		do {
			status = ydb_subscript_next_s(&var_sets[0], 3, &var_sets[1], &var_sets[3]);
			if(status == YDB_ERR_NODEEND)
				break;
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);
			var_sets[3].buf_addr[var_sets[3].len_used] = '\0';
			status = ydb_get_s(&var_sets[0], 3, &var_sets[1], &var_value);
			YDB_ERROR_CHECK(status, &z_status, &z_status_value);
			var_value.buf_addr[var_value.len_used] = '\0';
			message_parm.name = var_sets[3].buf_addr;
			message_parm.value = var_value.buf_addr;
			parameter_status = make_parameter_status(&message_parm);
			result = send_message(&rocto_session, (BaseMessage*)(&parameter_status->type));
			free(parameter_status);
			if(result) {
				return 0;
			}
		} while(TRUE);

		// Free temporary buffers
		free(var_defaults[2].buf_addr);
		free(var_value.buf_addr);
		free(var_defaults);
		free(var_sets);
		rocto_main_loop(&rocto_session);
		break;
	}

	if(thread_id != 0) {
		// We only want to close the sfd if we are the parent process which actually listens to
		// the socket; thread_id will be 0 if we are a child process
		close(sfd);
	}

	return 0;
}
