/****************************************************************
 *								*
 * Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	*
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
#include <sys/socket.h>
#include <sys/syscall.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <pthread.h>
#include <signal.h>
#include <netinet/tcp.h>

#include "octo.h"
#include "rocto/rocto.h"
#include "rocto/message_formats.h"
#include "helpers.h"

void handle_sigint(int sig, siginfo_t *info, void *context) {
	UNUSED(sig);
	UNUSED(info);
	UNUSED(context);
	rocto_session.session_ending = TRUE;
}

#if YDB_TLS_AVAILABLE
#include "ydb_tls_interface.h"
#endif

int main(int argc, char **argv) {
	AuthenticationMD5Password *md5auth;
	AuthenticationOk	  *authok;
	BackendKeyData		  *backend_key_data;
	BaseMessage		  *base_message;
	CancelRequest		  *cancel_request;
	ParameterStatus		  *parameter_status;
	PasswordMessage		  *password_message;
	SSLRequest		  *ssl_request;
	StartupMessage		  *startup_message;
	StartupMessageParm	   message_parm;
	char			  *buffer;
	char			   host_buf[NI_MAXHOST], serv_buf[NI_MAXSERV];
	int32_t			   buffer_size = OCTO_INIT_BUFFER_LEN;
	int32_t			   cur_parm = 0;
	int32_t			   sfd, cfd, opt, status = 0;
	int64_t			   mem_usage;
	pid_t			   child_id = 0;
	struct sigaction	   ctrlc_action;
	struct sockaddr_in	  *address = NULL;
	struct sockaddr_in6	   addressv6;
	ydb_buffer_t		   ydb_buffers[2];
	ydb_buffer_t		  *session_buffer = &(ydb_buffers[0]), *session_id_buffer;
	ydb_buffer_t		   z_interrupt, z_interrupt_handler;
	ydb_buffer_t		   pid_subs[2], timestamp_buffer;
	ydb_buffer_t		  *pid_buffer = &pid_subs[0];
	ydb_buffer_t		   pg_buffers[4];
	socklen_t		   addrlen;
	ydb_buffer_t		   secret_key_list_buffer, secret_key_buffer, value_buffer, variable_buffer;
	char pid_str[INT32_TO_STRING_MAX], secret_key_str[INT32_TO_STRING_MAX], timestamp_str[INT64_TO_STRING_MAX];
	int  secret_key = 0, done, read_only;
#if YDB_TLS_AVAILABLE
	gtm_tls_ctx_t *tls_context;
#endif

	// Initialize connection details in case errors prior to connections - needed before octo_init for Rocto error reporting
	rocto_session.ip = "IP_UNSET";
	rocto_session.port = "PORT_UNSET";

	/* Before invoking "ydb_init()" (inside "octo_init()") set env var to ensure SIGUSR2 is treated the same as SIGUSR1.
	 * This is needed by rocto so SIGUSR1 does creates ZSHOW dump files and SIGUSR2 cancels queries.
	 */
	setenv("ydb_treat_sigusr2_like_sigusr1", "1", TRUE);

	// Do "octo_init" first as it initializes an environment variable ("ydb_lv_nullsubs") and that needs to be done
	// before any "ydb_init" call happens (as the latter reads this env var to know whether to allow nullsubs in lv or not)
	status = octo_init(argc, argv);
	if (0 != status)
		return status;

	opt = addrlen = 0;

	// Create buffers for managing secret keys for CancelRequests
	YDB_LITERAL_TO_BUFFER(OCTOLIT_YDBOCTOSECRETKEYLIST, &secret_key_list_buffer);

	// Initialize a handler to respond to ctrl + c
	memset(&ctrlc_action, 0, sizeof(ctrlc_action));
	ctrlc_action.sa_flags = SA_SIGINFO;
	ctrlc_action.sa_sigaction = handle_sigint;
	status = sigaction(SIGINT, &ctrlc_action, NULL);
	if (0 != status)
		FATAL(ERR_SYSCALL, "sigaction(SIGINT)", errno, strerror(errno));

	status = ydb_init(); /* YDB init needed for signal handler setup and gtm_tls_init call below */
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		CLEANUP_CONFIG(config->config_file);
		return status;
	}
	YDB_LITERAL_TO_BUFFER("$ZINTERRUPT", &z_interrupt);
	YDB_LITERAL_TO_BUFFER("DO zintr^%ydboctoZinterrupt", &z_interrupt_handler);
	status = ydb_set_s(&z_interrupt, 0, NULL, &z_interrupt_handler);
	YDB_ERROR_CHECK(status);
	if (YDB_OK != status) {
		CLEANUP_CONFIG(config->config_file);
		return status;
	}

	rocto_session.session_ending = FALSE;

	// Disable sending log messages until all startup messages have been sent
	rocto_session.sending_message = TRUE;

	// Setup the address first so we know which protocol to use
	memset(&addressv6, 0, sizeof(struct sockaddr_in6));
	address = (struct sockaddr_in *)(&addressv6);
	address->sin_family = AF_INET;
	addrlen = sizeof(struct sockaddr_in6);
	status = inet_pton(AF_INET, config->rocto_config.address, &address->sin_addr);
	switch (status) {
	case 0:
		addressv6.sin6_family = AF_INET6;
		status = inet_pton(AF_INET6, config->rocto_config.address, &addressv6.sin6_addr);
		switch (status) {
		case 0:
			FATAL(ERR_ROCTO_BAD_ADDRESS, config->rocto_config.address);
			break;
		case 1:
			break;
		default:
			FATAL(ERR_SYSCALL, "inet_pton", errno, strerror(errno));
			break;
		}
		break;
	case 1:
		break;
	default:
		FATAL(ERR_SYSCALL, "inet_pton", errno, strerror(errno));
		break;
	}
	address->sin_port = htons(config->rocto_config.port);

	if (-1 == (sfd = socket(address->sin_family, SOCK_STREAM, 0))) {
		FATAL(ERR_SYSCALL, "socket", errno, strerror(errno));
	}
	opt = 1;
	if (-1 == setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt))) {
		FATAL(ERR_SYSCALL, "setsockopt", errno, strerror(errno));
	}

	if (!config->rocto_config.tcp_delay) {
		if (-1 == setsockopt(sfd, IPPROTO_TCP, TCP_NODELAY, &opt, sizeof(opt))) {
			FATAL(ERR_SYSCALL, "setsockopt", errno, strerror(errno));
		}
	}

	if (bind(sfd, (struct sockaddr *)&addressv6, sizeof(addressv6)) < 0) {
		FATAL(ERR_SYSCALL, "bind", errno, strerror(errno));
	}

	/* Spin off another thread to keep an eye on dead processes.
	 * But before that, make sure that thread does not catch any signals (e.g. SIGTERM).
	 * We want only the main process to catch those signals as that does invoke "ydb_eintr_handler()"
	 * in case of EINTR interrupting a long-running system call. If the thread gets the signal,
	 * the main process would then hang as it would have no clue that it has to terminate.
	 * So we block all signals, except those which could be generated from within the thread,
	 * then create the thread (so it blocks those signals) and then restore the original signal
	 * mask in the main process so it can continue to catch those signals.
	 */
	sigset_t block_worker, savemask;
	sigfillset(&block_worker);
	sigdelset(&block_worker, SIGSEGV);
	sigdelset(&block_worker, SIGKILL);
	sigdelset(&block_worker, SIGFPE);
	sigdelset(&block_worker, SIGBUS);
	status = pthread_sigmask(SIG_BLOCK, &block_worker, &savemask);
	if (0 != status) {
		FATAL(ERR_SYSCALL, "pthread_sigmask(SIG_BLOCK)", status, strerror(status));
	}

	/* Create thread with signals blocked so it does not receive any external signals */
	pthread_t thread_id;
	status = pthread_create(&thread_id, NULL, rocto_helper_waitpid, (void *)(&rocto_session));
	if (0 != status) {
		FATAL(ERR_SYSCALL, "pthread_create", status, strerror(status));
	}

	/* Restore signal mask in main process now that thread creation is done */
	status = pthread_sigmask(SIG_SETMASK, &savemask, NULL);
	if (0 != status) {
		FATAL(ERR_SYSCALL, "pthread_sigmask(SIG_SETMASK)", status, strerror(status));
	}

	if (listen(sfd, 3) < 0) {
		FATAL(ERR_SYSCALL, "listen", errno, strerror(errno));
	}
	/* Now that we have started listening on a port, print message to rocto log so whoever monitors the rocto log
	 * knows that rocto is now accepting connections. Issuing this message before the "listen" can result in
	 * a race condition where another process sees the INFO_ROCTO_STARTED message but tries to connect before the
	 * "listen" system call has started and gets a "Connection refused" error because there is no listener.
	 */
	INFO(INFO_ROCTO_STARTED, config->rocto_config.port);

#if YDB_TLS_AVAILABLE
	tls_context = INVALID_TLS_CONTEXT;
#endif
	ssl_request = NULL;
	buffer = (char *)malloc(sizeof(char) * buffer_size);
	while (!rocto_session.session_ending) {
		if ((cfd = accept(sfd, (struct sockaddr *)address, &addrlen)) < 0) {
			if (rocto_session.session_ending) {
				break;
			}
			if (EINTR == errno) {
				ydb_eintr_handler(); /* Needed to invoke YDB signal handler (for signal that caused
						      * EINTR) in a deferred but timely fashion.
						      */
				continue;
			}
			ERROR(ERR_SYSCALL, "accept", errno, strerror(errno));
			break;
		}

		// Create secret key and matching buffer
		syscall(SYS_getrandom, &secret_key, 4, 0);
		snprintf(secret_key_str, INT32_TO_STRING_MAX, "%u", secret_key);
		YDB_STRING_TO_BUFFER(secret_key_str, &secret_key_buffer);

		child_id = fork();
		if (0 != child_id) {
			uint64_t timestamp;

			/* Close client socket fd now that it has been passed on to child (rocto server) process.
			 * Need to do this before moving on to accept more connections in parent (rocto listener)
			 * to avoid a file descriptor leak (YDBOcto#739).
			 */
			close(cfd);
			if (0 > child_id) {
				ERROR(ERR_SYSCALL, "fork()", errno, strerror(errno));
				break;
			}
			// Create pid buffer
			snprintf(pid_str, INT32_TO_STRING_MAX, "%u", child_id);
			YDB_STRING_TO_BUFFER(pid_str, pid_buffer);
			// Add pid/secret key pair to list in listener/parent
			status = ydb_set_s(&secret_key_list_buffer, 1, pid_buffer, &secret_key_buffer);
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				/* We have already logged the error as part of the YDB_ERROR_CHECK invocation above.
				 * Move on and listen on the port for future connections.
				 */
				continue;
			}

			// Get timestamp of the new process
			timestamp = get_pid_start_time(child_id);
			if (0 != timestamp) {
				// Populate pid/timestamp buffers
				YDB_LITERAL_TO_BUFFER(OCTOLIT_TIMESTAMP, &pid_subs[1]);
				snprintf(timestamp_str, INT64_TO_STRING_MAX, "%zu", timestamp);
				YDB_STRING_TO_BUFFER(timestamp_str, &timestamp_buffer);
				// Add timestamp under PID key
				status = ydb_set_s(&secret_key_list_buffer, 2, &pid_subs[0], &timestamp_buffer);
				YDB_ERROR_CHECK(status);
				if (YDB_OK != status) {
					/* We have already logged the error as part of the YDB_ERROR_CHECK invocation above.
					 * Move on and listen on the port for future connections.
					 */
					continue;
				}
			}
			/* Else: "timestamp" is 0. This could mean
			 * a) normal return if "child_id" has already terminated OR
			 * b) an abnormal return if "child_id" is a live process and there was an error trying to
			 *	retrieve the creation timestamp of "child_id".
			 * In either case, we will not be able to process cancel requests for this pid due to the missing
			 * timestamp. But in case (a), it does not matter since the pid is dead. In case (b), an error message
			 * would have already been logged by "get_pid_start_time()" so we have a record of this incident.
			 * Nothing more to be done. Continue to listen on the port for future connections.
			 */
			continue;
		}

		// Reset thread id to identify it as child process
		thread_id = 0;

		// Reset process_id to point to the child (would be pointing to the parent pid till now)
		config->process_id = getpid();

		LOG_LOCAL_ONLY(INFO, INFO_ROCTO_SERVER_FORKED, config->process_id); // Record rocto server process

		// First we read the startup message, which has a special format
		// 2x32-bit ints
		rocto_session.connection_fd = cfd;
		rocto_session.ssl_active = FALSE;
		if (config->rocto_config.use_dns) {
			status
			    = getnameinfo((const struct sockaddr *)address, addrlen, host_buf, NI_MAXHOST, serv_buf, NI_MAXSERV, 0);
		} else {
			status = getnameinfo((const struct sockaddr *)address, addrlen, host_buf, NI_MAXHOST, serv_buf, NI_MAXSERV,
					     NI_NUMERICHOST | NI_NUMERICSERV);
		}
		if (0 != status) {
			ERROR(ERR_SYSCALL, "getnameinfo", errno, strerror(errno));
			break;
		}
		rocto_session.ip = host_buf;
		rocto_session.port = serv_buf;
		LOG_LOCAL_ONLY(INFO, INFO_CLIENT_CONNECTED, NULL);

		status = ydb_init(); // YDB init needed by gtm_tls_init call below
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			break;
		}
		// Establish the connection first
		rocto_session.session_id = NULL;
		read_bytes(&rocto_session, &buffer, &buffer_size, sizeof(int) * 2, TRUE);

		// Attempt TLS connection, if configured
		ssl_request = read_ssl_request(&rocto_session, buffer, sizeof(int) * 2);
		if ((NULL != ssl_request) && config->rocto_config.ssl_on) {
#if YDB_TLS_AVAILABLE
			int tls_errno;

			// gtm_tls_conn_info tls_connection;
			status = send_bytes(&rocto_session, "S", sizeof(char));
			if (0 != status) {
				ERROR(ERR_ROCTO_SEND_FAILED, "failed to send SSL confirmation byte");
				break;
			}
			// Initialize TLS context: load config, certs, keys, etc.
			tls_context = gtm_tls_init(GTM_TLS_API_VERSION, GTMTLS_OP_INTERACTIVE_MODE);
			if (INVALID_TLS_CONTEXT == tls_context) {
				tls_errno = gtm_tls_errno();
				if (0 < tls_errno) {
					ERROR(ERR_SYSCALL, "gtm_tls_errno()", tls_errno, strerror(tls_errno));
				} else {
					ERROR(ERR_ROCTO_TLS_INIT, GTM_TLS_GET_ERROR(NULL));
				}
				break;
			}
			// Set up TLS socket
			gtm_tls_socket_t *tls_socket;
			tls_socket = gtm_tls_socket(tls_context, NULL, cfd, "OCTOSERVER", GTMTLS_OP_SOCKET_DEV);
			if (INVALID_TLS_SOCKET == tls_socket) {
				tls_errno = gtm_tls_errno();
				if (0 < tls_errno) {
					ERROR(ERR_SYSCALL, GTM_TLS_GET_ERROR(NULL), tls_errno, strerror(tls_errno));
				} else {
					ERROR(ERR_ROCTO_TLS_SOCKET, GTM_TLS_GET_ERROR(NULL));
				}
				break;
			}
			// Accept incoming TLS connections
			do {
				status = gtm_tls_accept(tls_socket);
				if (0 != status) {
					if (-1 == status) {
						tls_errno = gtm_tls_errno();
						if (-1 == tls_errno) {
							ERROR(ERR_ROCTO_TLS_ACCEPT, GTM_TLS_GET_ERROR(tls_socket));
						} else {
							ERROR(ERR_ROCTO_TLS_UNNAMED, strerror(tls_errno), tls_errno);
						}
						break;
					} else if (GTMTLS_WANT_READ == status) {
						ERROR(ERR_ROCTO_TLS_WANT_READ, NULL);
					} else if (GTMTLS_WANT_WRITE == status) {
						ERROR(ERR_ROCTO_TLS_WANT_WRITE, NULL);
					} else {
						ERROR(ERR_ROCTO_TLS_UNKNOWN, "failed to accept incoming connection(s)");
						break;
					}
				}
			} while ((GTMTLS_WANT_READ == status) || (GTMTLS_WANT_WRITE == status));
			rocto_session.tls_socket = tls_socket;
			rocto_session.ssl_active = TRUE;
			read_bytes(&rocto_session, &buffer, &buffer_size, sizeof(int) * 2, TRUE);
#endif
			// Attempt unencrypted connection if SSL is disabled
		} else if ((NULL != ssl_request) && !config->rocto_config.ssl_on) {
			status = send_bytes(&rocto_session, "N", sizeof(char));
			read_bytes(&rocto_session, &buffer, &buffer_size, sizeof(int) * 2, TRUE);
		} else if ((NULL == ssl_request) && config->rocto_config.ssl_required) {
			// Do not continue if TLS/SSL is required, but not requested by the client
			rocto_session.sending_message = FALSE; // Must enable message sending for client to be notified
			FATAL(ERR_ROCTO_TLS_REQUIRED, "");
			break;
		}

		// Check for CancelRequest and handle if so
		cancel_request = read_cancel_request(&rocto_session, &buffer, &buffer_size);
		if (NULL != cancel_request) {
			LOG_LOCAL_ONLY(INFO, ERR_ROCTO_QUERY_CANCELED, "");
			handle_cancel_request(cancel_request);
			// Shutdown connection immediately to free client prompt
			shutdown(rocto_session.connection_fd, SHUT_RDWR);
			free(cancel_request);
			break;
		}

		// Get actual (non-zero) pid of child/server
		child_id = config->process_id;
		snprintf(pid_str, INT32_TO_STRING_MAX, "%u", child_id);
		YDB_STRING_TO_BUFFER(pid_str, pid_buffer);
		// Clear all other secret key/pid pairs from other servers
		status = ydb_delete_s(&secret_key_list_buffer, 0, NULL, YDB_DEL_TREE);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			break;
		}
		// Add pid/secret key pair to list in server/child
		status = ydb_set_s(&secret_key_list_buffer, 1, pid_buffer, &secret_key_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			break;
		}

		startup_message = read_startup_message(&rocto_session, &buffer, &buffer_size);
		if (NULL == startup_message) {
			break;
		}
		// Require md5 authentication
		char salt[4];
		md5auth = make_authentication_md5_password(&rocto_session, salt);
		status = send_message(&rocto_session, (BaseMessage *)(&md5auth->type));
		if (status) {
			ERROR(ERR_ROCTO_SEND_FAILED, "failed to send MD5 authentication required");
			free(md5auth);
			free(startup_message->parameters);
			free(startup_message);
			break;
		}
		free(md5auth);

		// This next message is the user sending the password
		int rocto_err = 0;
		base_message = read_message(&rocto_session, &buffer, &buffer_size, &rocto_err);
		if (NULL == base_message) {
			if (-2 != rocto_err) {
				ERROR(ERR_ROCTO_READ_FAILED, "failed to read MD5 password");
			}
			free(startup_message->parameters);
			free(startup_message);
			break;
		}
		password_message = read_password_message(base_message);
		if (NULL == password_message) {
			free(startup_message->parameters);
			free(startup_message);
			break;
		}

		rocto_session.sending_message = FALSE;

		// Validate user credentials
		status = handle_password_message(password_message, startup_message, salt);
		free(password_message);
		if (0 != status) {
			free(startup_message->parameters);
			free(startup_message);
			break;
		}

		// Ok
		authok = make_authentication_ok();
		send_message(&rocto_session, (BaseMessage *)(&authok->type));
		free(authok);

		// Enter the main loop
		session_id_buffer = &(ydb_buffers[1]);
		YDB_STRING_TO_BUFFER(config->global_names.session, session_buffer);
		OCTO_MALLOC_NULL_TERMINATED_BUFFER(session_id_buffer, INT64_TO_STRING_MAX);
		/* Currently, the session_id will always be 1, since this incrementation is performed by each rocto server process
		 * *after* it is spawned from the parent "listener" process. This is acceptable as Octo currently has no
		 * session-specific functionality. However, PostgreSQL does have such functionality (see
		 * https://www.postgresql.org/docs/11/functions-info.html), so Octo may need to support
		 * this in the future. Accordingly, this code is retained in the hope that it will be useful if session-specific
		 * functionality ever becomes necessary.
		 */
		status = ydb_incr_s(session_buffer, 0, NULL, NULL, session_id_buffer);
		YDB_ERROR_CHECK(status);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(session_id_buffer);
			free(startup_message->parameters);
			free(startup_message);
			break;
		}
		rocto_session.session_id = session_id_buffer;
		rocto_session.session_id->buf_addr[rocto_session.session_id->len_used] = '\0';

		/* Override default run-time parameter variables with those specified by the client.
		 * Default run-time parameters were already loaded at startup by load_pg_defaults() in octo_init().
		 */
		for (cur_parm = 0; cur_parm < startup_message->num_parameters; cur_parm++) {
			char *name;
			/* Convert to lowercase for parameter name lookup. This is needed since StartupMessages do not pass through
			 * the lexer and so do not automatically get lowercased. Use the struct packing of StartupMessageParm to
			 * identify the end of the parameter name without needing to do a strlen().
			 */
			name = startup_message->parameters[cur_parm].name;
			TOLOWER_STR(name);
			name = startup_message->parameters[cur_parm].name;
			status = set_parameter_in_pg_settings(name, startup_message->parameters[cur_parm].value);
			if (YDB_OK != status) {
				if (2 == status) {
					/* The parameter name was "user" or "database", and so should not be stored in the database
					 * since these are not valid runtime parameters but authentication-only parameters. So, just
					 * continue to the next parameter.
					 */
					status = YDB_OK;
					continue;
				}
				break;
			}
		}
		free(startup_message->parameters);
		free(startup_message);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(session_id_buffer);
			break;
		}

		/* Prepare buffers for looping over runtime parameter LVNS, i.e.:
		 *	%ydboctoocto(OCTOLIT_SETTINGS,OCTOLIT_PG_SETTINGS,NAME_LOWER)=NAME
		 */
		YDB_STRING_TO_BUFFER(config->global_names.raw_octo, &pg_buffers[0]);
		YDB_STRING_TO_BUFFER(OCTOLIT_SETTINGS, &pg_buffers[1]);
		YDB_STRING_TO_BUFFER(OCTOLIT_PG_SETTINGS, &pg_buffers[2]);
		// Prepare buffers for storing runtime parameter names during iteration over list of valid parameter names
		OCTO_MALLOC_NULL_TERMINATED_BUFFER(&variable_buffer, OCTO_INIT_BUFFER_LEN);
		OCTO_MALLOC_NULL_TERMINATED_BUFFER(&pg_buffers[3], OCTO_INIT_BUFFER_LEN);
		YDB_COPY_STRING_TO_BUFFER("", &pg_buffers[3], done);
		if (!done) {
			YDB_FREE_BUFFER(session_id_buffer);
			YDB_FREE_BUFFER(&pg_buffers[3]);
			break;
		}
		/* Send ParameterStatus messages to client for each set runtime parameter,
		 * i.e. each one with a value other than the empty string.
		 */
		read_only = FALSE; /* Indicates whether ydb_subscript_next_s is looping over read-only run-time variables. See
				    * LOAD_READ_ONLY_VARIABLE in pg_defaults.h and get_parameter_from_pg_settings.c for more
				    * information.
				    */
		do {
			int save_len_used;
			save_len_used = pg_buffers[3].len_used; /* take a copy before "ydb_subscript_next_s()" modifies it */

			status = ydb_subscript_next_s(&pg_buffers[0], 3, &pg_buffers[1], &pg_buffers[3]);
			if (YDB_ERR_INVSTRLEN == status) {
				/* "ydb_subscript_next_s()" would have overwritten "pg_buffers[3].len_used" to say needed space.
				 * Allocate a new buffer with that space. Then copy old buffer over to new buffer.
				 * And then free old buffer.
				 */
				ydb_buffer_t tmp_buff;
				tmp_buff = pg_buffers[3];
				OCTO_MALLOC_NULL_TERMINATED_BUFFER(&pg_buffers[3], tmp_buff.len_used);
				tmp_buff.len_used = save_len_used; /* restore original "len_used" from saved copy */
				YDB_COPY_BUFFER_TO_BUFFER(&tmp_buff, &pg_buffers[3], done);
				assert(done);
				YDB_FREE_BUFFER(&tmp_buff);
				/* Now redo the "ydb_subscript_next_s()" */
				status = ydb_subscript_next_s(&pg_buffers[0], 3, &pg_buffers[1], &pg_buffers[3]);
				assert(YDB_ERR_INVSTRLEN != status);
			}
			if (YDB_ERR_NODEEND == status) {
				if (FALSE == read_only) {
					/* We haven't yet sent ParameterStatus messages for read-only runtime variables, which are
					 * stored in a different LVN, i.e.:
					 *	%ydboctoocto(OCTOLIT_SETTINGS,OCTOLIT_READ_ONLY,NAME_LOWER)
					 * So, loop over these next.
					 */
					YDB_STRING_TO_BUFFER(OCTOLIT_READ_ONLY, &pg_buffers[2]);
					YDB_COPY_STRING_TO_BUFFER("", &pg_buffers[3], done);
					if (!done) {
						status = (!YDB_OK);
						break;
					}
					read_only = TRUE;
					continue;
				}
				status = YDB_OK;
				break;
			}
			YDB_ERROR_CHECK(status);
			if (YDB_OK != status) {
				break;
			}
			pg_buffers[3].buf_addr[pg_buffers[3].len_used] = '\0';
			message_parm.name = pg_buffers[3].buf_addr;
			message_parm.value = get_parameter_from_pg_settings(&message_parm.name, &value_buffer);
			if (NULL == message_parm.value) {
				break;
			}
			if (0 == value_buffer.len_used) {
				/* There's no value for this parameter, but it exists. Just continue without sending a
				 * ParameterStatus message.
				 */
				free(message_parm.value); // Allocated in get_parameter_from_pg_settings
				continue;
			}
			parameter_status = make_parameter_status(&message_parm);
			status = send_message(&rocto_session, (BaseMessage *)(&parameter_status->type));
			LOG_LOCAL_ONLY(INFO, INFO_ROCTO_PARAMETER_STATUS_SENT, message_parm.name, message_parm.value);
			YDB_FREE_BUFFER(&value_buffer); // Allocated in get_parameter_from_pg_settings
			free(parameter_status);
			if (status) {
				break;
			}
		} while (TRUE);
		YDB_FREE_BUFFER(&variable_buffer);
		YDB_FREE_BUFFER(&pg_buffers[3]);
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(session_id_buffer);
			break;
		}

		// Clean up after any errors from the above loop
		assert(0 == YDB_OK); // or else if `status` evaluated to true in the above loop, we would try to send another
				     // message on error
		if (YDB_OK != status) {
			YDB_FREE_BUFFER(session_id_buffer);
			break;
		}

		// Send secret key info to client
		backend_key_data = make_backend_key_data(secret_key, child_id);
		status = send_message(&rocto_session, (BaseMessage *)(&backend_key_data->type));
		free(backend_key_data);
		if (status) {
			YDB_FREE_BUFFER(session_id_buffer);
			break;
		}

		// Get user permissions and store on rocto_session for reference during query handling
		status = get_user_permissions(&rocto_session);
		if (status) {
			YDB_FREE_BUFFER(session_id_buffer);
			break;
		}

		rocto_main_loop(&rocto_session);
		YDB_FREE_BUFFER(session_id_buffer);
		break;
	}
	if (0 != thread_id) {
		// We only want to close the sfd if we are the parent process which actually listens to
		// the socket; thread_id will be 0 if we are a child process
		close(sfd);
		status = pthread_join(thread_id, NULL);
		if (0 != status) {
			ERROR(ERR_SYSCALL, "pthread_join()", status, strerror(status));
		}
	}
	// Since each iteration of the loop spawns a child process, each of which calls `gtm_tls_init`,
	// we call `gtm_tls_fini` for each child.
#if YDB_TLS_AVAILABLE
	if (INVALID_TLS_CONTEXT != tls_context) {
		gtm_tls_fini(&tls_context);
	}
#endif
	if (NULL != ssl_request) {
		free(ssl_request);
	}

	if (TRACE >= config->verbosity_level) {
		mem_usage = get_mem_usage();
		if (0 <= mem_usage) {
			TRACE(INFO_MEMORY_USAGE, mem_usage);
		} else {
			ERROR(ERR_MEMORY_USAGE, "");
		}
		status = ydb_ci("_ydboctoNodeDump");
		YDB_ERROR_CHECK(status);
	}

	CLEANUP_CONFIG(config->config_file);
	free(buffer);
	return status;
}
