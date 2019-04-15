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
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "message_formats.h"


CommandComplete *make_command_complete(char *command_tag) {
	CommandComplete *ret;
	char *c;
	int length = 0, command_tag_length = 0;

	// Rather than have special logic for the NULL, just use an empty string
	if(command_tag == NULL)
		command_tag = "";

	length += sizeof(unsigned int);
	command_tag_length = strlen(command_tag) + 1;	// count null
	length += command_tag_length;
	ret = (CommandComplete*)malloc(length + sizeof(CommandComplete) - sizeof(unsigned int));
	memset(ret, 0, length + sizeof(CommandComplete) - sizeof(unsigned int));

	ret->type = PSQL_CommandComplete;
	ret->length = htonl(length);
	memcpy(ret->data, command_tag, command_tag_length);

	return ret;
}
