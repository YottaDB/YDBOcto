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

#if YDB_TLS_AVAILABLE
#include "ydb_tls_interface.h"
#endif

int main(int argc, char **argv) {
	AuthenticationMD5Password	*md5auth;
	AuthenticationOk		*authok;
	BaseMessage			*base_message;
	ErrorBuffer			err_buff;
	ErrorResponse			*err;
	ParameterStatus			*parameter_status;
	PasswordMessage			*password_message;
	SSLRequest			*ssl_request;
	StartupMessage			*startup_message;
	StartupMessageParm		message_parm;
	char				buffer[MAX_STR_CONST];
	char				host_buf[NI_MAXHOST], serv_buf[NI_MAXSERV];
	const char			*err_str;
	const char			*error_message;
	int				cur_parm = 0, i = 0;
	int				sfd, cfd, opt, addrlen, result, status;
	int				tls_errno;
	pid_t				child_id = 0;
	struct sigaction		ctrlc_action;
	struct sockaddr_in		*address = NULL;
	struct sockaddr_in6		addressv6;

	sfd = cfd = opt = addrlen = result = status = 0;
	err_buff.offset = 0;

	ydb_buffer_t ydb_buffers[2], *var_defaults, *var_sets, var_value;
	ydb_buffer_t *session_buffer = &(ydb_buffers[0]), *session_id_buffer = &(ydb_buffers[1]);

	// Initialize a handler to respond to ctrl + c
	memset(&ctrlc_action, 0, sizeof(ctrlc_action));
	ctrlc_action.sa_flags = SA_SIGINFO;
	ctrlc_action.sa_sigaction = (void *)handle_sigint;
	sigaction(SIGINT, &ctrlc_action, NULL);

	// Initialize connection details in case errors prior to connections
	rocto_session.ip = "IP_UNSET";
	rocto_session.port = "PORT_UNSET";

	status = octo_init(argc, argv);
	if (0 != status) {
		return status;
	}
	INFO(INFO_ROCTO_STARTED, config->rocto_config.port);

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

	if((sfd = socket(address->sin_family, SOCK_STREAM, 0)) == -1) {
		FATAL(ERR_SYSCALL, "socket", errno, strerror(errno));
	}

	opt = 1;
	if(setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt)) == -1) {
		FATAL(ERR_SYSCALL, "setsockopt", errno, strerror(errno));
	}

	if(bind(sfd, (struct sockaddr *)&addressv6, sizeof(addressv6)) < 0) {
		FATAL(ERR_SYSCALL, "bind", errno, strerror(errno));
	}

	// Spin off another thread to keep an eye on dead processes
	pthread_t thread_id;
	pthread_create(&thread_id, NULL, rocto_helper_waitpid, (void*)(&rocto_session));

	if(listen(sfd, 3) < 0) {
		FATAL(ERR_SYSCALL, "listen", errno, strerror(errno));
	}
	while (!rocto_session.session_ending) {
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
		rocto_session.ssl_active = FALSE;
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

		status = ydb_init();		// YDB init needed by gtm_tls_init call below */
		YDB_ERROR_CHECK(status);

		// Establish the connection first
		rocto_session.session_id = NULL;
		read_bytes(&rocto_session, buffer, MAX_STR_CONST, sizeof(int) * 2);
		// Attempt SSL connection, if configured
		ssl_request = read_ssl_request(&rocto_session, buffer, sizeof(int) * 2, &err);
		if (NULL != ssl_request & TRUE == config->rocto_config.ssl_on) {
#			if YDB_TLS_AVAILABLE
			// gtm_tls_conn_info tls_connection;
			result = send_bytes(&rocto_session, "S", sizeof(char));
			if (0 != result) {
				WARNING(ERR_ROCTO_SEND_FAILED, "failed to send SSL confirmation byte");
				break;
			}
			// Initialize TLS context: load config, certs, keys, etc.
			gtm_tls_ctx_t *tls_context;
			tls_context = gtm_tls_init(GTM_TLS_API_VERSION, GTMTLS_OP_INTERACTIVE_MODE);
			if (INVALID_TLS_CONTEXT == tls_context) {
				tls_errno = gtm_tls_errno();
				if (0 < tls_errno) {
					err_str = gtm_tls_get_error();
					WARNING(ERR_SYSCALL, err_str, tls_errno, strerror(tls_errno));
				} else {
					err_str = gtm_tls_get_error();
					WARNING(ERR_ROCTO_TLS_INIT, err_str);
				}
				break;
			}
			// Set up TLS socket
			gtm_tls_socket_t *tls_socket;
 			tls_socket = gtm_tls_socket(tls_context, NULL, cfd, "OCTOSERVER", GTMTLS_OP_SOCKET_DEV);
			if (INVALID_TLS_SOCKET == tls_socket) {
				tls_errno = gtm_tls_errno();
				if (0 < tls_errno) {
					err_str = gtm_tls_get_error();
					WARNING(ERR_SYSCALL, err_str, tls_errno, strerror(tls_errno));
				} else {
					err_str = gtm_tls_get_error();
					WARNING(ERR_ROCTO_TLS_SOCKET, err_str);
				}
				break;
			}
			// Accept incoming TLS connections
			do {
				result = gtm_tls_accept(tls_socket);
				if (0 != result) {
					if (-1 == result) {
						tls_errno = gtm_tls_errno();
						if (-1 == tls_errno) {
							err_str = gtm_tls_get_error();
							WARNING(ERR_ROCTO_TLS_ACCEPT, err_str);
						} else {
							WARNING(CUSTOM_ERROR, "unknown", tls_errno, strerror(tls_errno));
						}
						break;
					} else if (GTMTLS_WANT_READ == result) {
						WARNING(ERR_ROCTO_TLS_WANT_READ);
					} else if (GTMTLS_WANT_WRITE == result) {
						WARNING(ERR_ROCTO_TLS_WANT_WRITE);
					} else {
						WARNING(ERR_ROCTO_TLS_UNKNOWN, "failed to accept incoming connection(s)");
						break;
					}
				}
			} while ((GTMTLS_WANT_READ == result) || (GTMTLS_WANT_WRITE == result));
			rocto_session.tls_socket = tls_socket;
			rocto_session.ssl_active = TRUE;
			read_bytes(&rocto_session, buffer, MAX_STR_CONST, sizeof(int) * 2);
#			endif
		// Attempt unencrypted connection if SSL not requested
		} else if (NULL != ssl_request & FALSE == config->rocto_config.ssl_on) {
			result = send_bytes(&rocto_session, "N", sizeof(char));
			read_bytes(&rocto_session, buffer, MAX_STR_CONST, sizeof(int) * 2);
		}

		startup_message = read_startup_message(&rocto_session, buffer, sizeof(int) * 2, &err);
		if(startup_message == NULL) {
			send_message(&rocto_session, (BaseMessage*)(&err->type));
			free(err);
			break;
		}
		// Require md5 authentication
		char salt[4];
		md5auth = make_authentication_md5_password(&rocto_session, salt);
		result = send_message(&rocto_session, (BaseMessage*)(&md5auth->type));
		if(result) {
			WARNING(ERR_ROCTO_SEND_FAILED, "failed to send MD5 authentication required");
			error_message = format_error_string(&err_buff, ERR_ROCTO_SEND_FAILED,
					"failed to send MD5 authentication required");
			err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Protocol_Violation,
						   error_message,
						   0);
			send_message(&rocto_session, (BaseMessage*)(&err->type));
			free(err);
			free(md5auth);
			break;
		}
		free(md5auth);

		// This next message is the user sending the password
		int rocto_err = 0;
		base_message = read_message(&rocto_session, buffer, MAX_STR_CONST, &rocto_err);
		if(base_message == NULL) {
			if (-2 != rocto_err) {
				WARNING(ERR_ROCTO_READ_FAILED, "failed to read MD5 password");
				error_message = format_error_string(&err_buff, ERR_ROCTO_READ_FAILED, "failed to read MD5 password");
				err = make_error_response(PSQL_Error_ERROR,
							   PSQL_Code_Protocol_Violation,
							   error_message,
							   0);
				send_message(&rocto_session, (BaseMessage*)(&err->type));
				free(err);
			}
			break;
		}
		password_message = read_password_message(base_message, &err);
		if(password_message == NULL) {
			send_message(&rocto_session, (BaseMessage*)(&err->type));
			free_error_response(err);
			break;
		}
		// Validate user credentials
		result = handle_password_message(password_message, &err, startup_message, salt);
		if (0 != result) {
			send_message(&rocto_session, (BaseMessage*)(&err->type));
			free_error_response(err);
			break;
		}

		// Ok
		authok = make_authentication_ok();
		send_message(&rocto_session, (BaseMessage*)(&authok->type));
		free(authok);

		rocto_session.sending_message = FALSE;

		// Enter the main loop
		session_buffer = &(ydb_buffers[0]);
		session_id_buffer = &(ydb_buffers[1]);
		YDB_STRING_TO_BUFFER(config->global_names.session, session_buffer);
		YDB_MALLOC_BUFFER(session_id_buffer, MAX_STR_CONST);
		status = ydb_incr_s(session_buffer, 0, NULL, NULL, session_id_buffer);
		YDB_ERROR_CHECK(status);
		rocto_session.session_id = session_id_buffer;
		rocto_session.session_id->buf_addr[rocto_session.session_id->len_used] = '\0';

		// Populate default parameters
		var_defaults = make_buffers(config->global_names.octo, 2, "variables", "");
		YDB_MALLOC_BUFFER(&var_defaults[2], MAX_STR_CONST);
		YDB_MALLOC_BUFFER(&var_value, MAX_STR_CONST);
		var_sets = make_buffers(config->global_names.session, 3, rocto_session.session_id->buf_addr,
				"variables", "");
		var_sets[3] = var_defaults[2];
		do {
			status = ydb_subscript_next_s(&var_defaults[0], 2, &var_defaults[1], &var_defaults[2]);
			if(status == YDB_ERR_NODEEND)
				break;
			YDB_ERROR_CHECK(status);
			status = ydb_get_s(&var_defaults[0], 2, &var_defaults[1], &var_value);
			YDB_ERROR_CHECK(status);
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
			YDB_ERROR_CHECK(status);
			var_sets[3].buf_addr[var_sets[3].len_used] = '\0';
			status = ydb_get_s(&var_sets[0], 3, &var_sets[1], &var_value);
			YDB_ERROR_CHECK(status);
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
