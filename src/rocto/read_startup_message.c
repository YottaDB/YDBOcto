/* Copyright (C) 2018-2019 YottaDB, LLC
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
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "rocto.h"
#include "message_formats.h"

StartupMessage *read_startup_message(RoctoSession *session, char *data, int data_length, ErrorResponse **err) {
	StartupMessage *ret = NULL;
	int num_parms = 0, read = 0, cur_parm = 0;
	char *c, *message_end;
	// Length plus protocol version
	unsigned int hard_coded_ints = sizeof(unsigned int) + sizeof(unsigned int);

	// First read length and protocol type, then we will reallocate things
	ret = (StartupMessage*)malloc(sizeof(StartupMessage));
	memcpy(&ret->length, data, hard_coded_ints);

	// Protocol version number format:
	// 	most significant 16 bits:  major version #, i.e. 3
	// 	least significant 16 bits: minor version #, i.e. 0
	if(ntohl(ret->protocol_version) != 0x00030000) {
		*err = make_error_response(PSQL_Error_FATAL,
					   PSQL_Code_Protocol_Violation,
					   "Protocol version did not match expected",
					   0);
		free(ret);
		return NULL;
	}

	/*if(ntohl(ret->length) != data_length) {
		// The routine starting this up should fill this out, but really
		//  we expect it to always be the same since both processes are
		//  looking at the same data
		*err = make_error_response(PSQL_Error_PANIC,
					   PSQL_Code_Protocol_Violation,
					   "Length read does match what is expected",
					   0);
		free(ret);
		return NULL;
	}*/

	// No parameters send
	if(ntohl(ret->length) == hard_coded_ints) {
		return ret;
	}

	// Prepare to reallocate
	data_length = ntohl(ret->length);
	free(ret);

	// Size is length in packet + other stuff in the struct, minus the hard-coded
	//  elements in the struct (two int4's)
	ret = (StartupMessage*)malloc(sizeof(StartupMessage) + data_length - hard_coded_ints);
	memcpy(&ret->length, data, hard_coded_ints);
	read_bytes(session, (char*)&ret->data, data_length - hard_coded_ints, data_length - hard_coded_ints);

	c = ret->data;
	message_end = (char*)(&ret->length) + data_length;

	// Fill out the list of messages; first, count them
	while(c < message_end && *c != '\0') {
		// Read parameter name
		for(; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		if(c == message_end || *c != '\0') {
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Protocol_Violation,
						   "Non-terminated parameter name",
						   0);
			free(ret);
			return NULL;
		}
		c++;
		// Read parameter value
		for(; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		if(c == message_end || *c != '\0') {
			*err = make_error_response(PSQL_Error_ERROR,
						   PSQL_Code_Protocol_Violation,
						   "Non-terminated parameter value",
						   0);
			free(ret);
			return NULL;
		}
		c++;
		num_parms++;
	}
	// Skip over the ending \0
	c++;

	// If there are trailing characters, note it
	//  Right now, we will abort the startup, but it's possible to continue in this case
	if(c != message_end) {
		*err = make_error_response(PSQL_Error_ERROR,
					   PSQL_Code_Protocol_Violation,
					   "Trailing characters after startup message",
					   0);
		free(ret);
		return NULL;
	}

	// Allocate parameter spots, reset c, scan through and populate data
	ret->parameters = (StartupMessageParm*)malloc(sizeof(StartupMessageParm) * num_parms);
	c = ret->data;
	cur_parm = 0;

	while(c < message_end && *c != '\0') {
		// Read parameter name
		ret->parameters[cur_parm].name = c;
		for(; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		// We already checked this message, so don't expect an error case
		assert(*c == '\0');
		c++;
		ret->parameters[cur_parm].value = c;
		// Read parameter value
		for(; c < message_end && *c != '\0'; c++) {
			// Left blank
		}
		assert(*c == '\0');
		c++;
		cur_parm++;
	}

	return ret;
}
