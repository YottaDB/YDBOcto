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

// Used to convert between network and host endian
#include <arpa/inet.h>

#include <openssl/md5.h>

#include <libyottadb.h>

#include "rocto.h"
#include "message_formats.h"

PasswordMessage *make_password_message(char *user, char *password, char *salt) {
	PasswordMessage *ret;
	int length = 0;

	// Rather than have special logic for the NULL, just use an empty string
	if(password == NULL) {
		password = "";
	}

	// Concatenate password and user
	unsigned char hash_buf[MAX_STR_CONST];
	int result = sprintf((char*)hash_buf, "%s%s", password, user);
	if (0 > result) {
		return NULL;
	}
	// Hash password and user
	MD5(hash_buf, strlen((char*)hash_buf), hash_buf);
	// Convert hash to hex string
	unsigned int hex_hash_len = MD5_DIGEST_LENGTH * 2 + 1;		// count null
	char hex_hash[hex_hash_len];
	result = md5_to_hex(hash_buf, hex_hash, hex_hash_len);
	if (0 != result) {
		return NULL;
	}

	// Concatenate password/user hash with salt
	result = snprintf((char*)hash_buf, hex_hash_len + 4, "%s%s", hex_hash, salt);	// Exclude "md5" prefix
	if (0 > result) {
		return NULL;
	}
	// Hash password/user hash with salt
	MD5(hash_buf, strlen((char*)hash_buf), hash_buf);
	// Convert hash to hex string
	result = md5_to_hex(hash_buf, hex_hash, hex_hash_len);
	if (0 != result) {
		return NULL;
	}

	// Add "md5" prefix to hex hash for transmission
	unsigned int md5_password_len = hex_hash_len + 3;
	char md5_password[md5_password_len];
	result = snprintf(md5_password, md5_password_len, "%s%s", "md5", hex_hash);
	if (0 > result) {
		return NULL;
	}

	length += sizeof(unsigned int);
	length += md5_password_len;
	ret = (PasswordMessage*)malloc(length + sizeof(PasswordMessage) - sizeof(unsigned int));
	memset(ret, 0, length + sizeof(PasswordMessage) - sizeof(unsigned int));

	ret->type = PSQL_PasswordMessage;
	ret->length = htonl(length);
	memcpy(ret->data, md5_password, md5_password_len);
	ret->password = ret->data;

	return ret;
}
