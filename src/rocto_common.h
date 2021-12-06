/****************************************************************
 *								*
 * Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

#include <libyottadb.h>

#if YDB_TLS_AVAILABLE
#include "ydb_tls_interface.h"
#endif

#define ROCTO_COMMON

typedef struct {
	int32_t	      connection_fd;
	int32_t	      sending_message;
	char *	      ip;
	char *	      port;
	ydb_buffer_t *session_id;
	int32_t	      session_ending;
	int32_t	      ssl_active;
	int32_t	      pid;
	int32_t	      secret_key;
	int32_t	      permissions;
	char	      username[OCTO_MAX_IDENT + 1]; // Null terminator
#if YDB_TLS_AVAILABLE
	gtm_tls_socket_t *tls_socket;
#endif
} RoctoSession;

typedef enum UserPermissions {
	UserPermissions_ReadOnly,
	UserPermissions_ReadWrite,
	UserPermissions_ROAllowSchemaChanges,
	UserPermissions_RWAllowSchemaChanges,
} UserPermissions_t;

// Global
extern RoctoSession rocto_session;
