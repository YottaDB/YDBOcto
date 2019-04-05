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


ParameterStatus *make_parameter_status(StartupMessageParm *parm) {
	unsigned int length;
	ParameterStatus *ret;
	char *c;
	int name_len, value_len;

	length = 0;
	length += sizeof(unsigned int);
	name_len = strlen(parm->name);
	length += name_len + 1;
	value_len = strlen(parm->value);
	length += value_len + 1;

	// malloc space for everything, but don't count lenght twice
	ret = (ParameterStatus*)malloc(sizeof(ParameterStatus) + length - sizeof(unsigned int));
	memset(ret, 0, sizeof(ParseComplete));

	ret->type = PSQL_ParameterStatus;
	ret->length = htonl(length);
	c = ret->data;
	memcpy(c, parm->name, name_len);
	c += name_len;
	*c++ = '\0';
	memcpy(c, parm->value, value_len);
	c += value_len;
	*c++ = '\0';

	return ret;
}
