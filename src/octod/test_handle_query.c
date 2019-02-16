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
#include <stdio.h>
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

// Used to convert between network and host endian
#include <arpa/inet.h>

#include "octo.h"
#include "octod.h"
#include "message_formats.h"
#include "helpers.h"

int __wrap_send_message(OctodSession *session, BaseMessage *message) {
	int message_type = mock_type(char);
	assert_true(message_type == message->type);
	return 0;
}

static void test_valid_input(void **state) {
	Query *query;
	OctodSession session;
	char *query_text = "SELECT * FROM names WHERE firstName = \"Acid\";", *c;
	int result;

	query = (Query*)malloc(sizeof(Query) + strlen(query_text) + 1);
	c = query->data;
	memcpy(c, query_text, strlen(query_text));
	c += strlen(query_text);
	*c++ = '\0';
	query->type = PSQL_Query;
	query->query = query->data;

	will_return(__wrap_send_message, PSQL_RowDescription);
	will_return(__wrap_send_message, PSQL_DataRow);
	will_return(__wrap_send_message, PSQL_CommandComplete);
	result = handle_query(query, &session);

	assert_int_equal(result, 0);
}

static void test_bad_sql(void **state) {
	Query *query;
	OctodSession session;
	char *query_text = "SELECT * FOREVERMORE names WHERE firstName = \"Acid\";", *c;
	int result;

	query = (Query*)malloc(sizeof(Query) + strlen(query_text) + 1);
	c = query->data;
	memcpy(c, query_text, strlen(query_text));
	c += strlen(query_text);
	*c++ = '\0';
	query->type = PSQL_Query;
	query->query = query->data;

	will_return(__wrap_send_message, PSQL_ErrorResponse);
	result = handle_query(query, &session);

	assert_int_equal(result, 0);
}

int main(void) {
	octo_init();
	const struct CMUnitTest tests[] = {
		   cmocka_unit_test(test_valid_input),
		   cmocka_unit_test(test_bad_sql)
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
