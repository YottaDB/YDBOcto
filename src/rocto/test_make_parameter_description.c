/****************************************************************
 *								*
 * Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	*
 * All rights reserved.						*
 *								*
 *	This source code contains the intellectual property	*
 *	of its copyright holder(s), and is made available	*
 *	under a license.  If you do not know the terms of	*
 *	the license, please stop and do not read further.	*
 *								*
 ****************************************************************/

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

#include "rocto.h"
#include "helpers.h"
#include "message_formats.h"

int __wrap_ydb_get_s(ydb_buffer_t *varname, int32_t subs_used, ydb_buffer_t *subsarray, ydb_buffer_t *ret_value) {
	if (0 == strncmp(varname->buf_addr, "$ZGBLDIR", varname->len_used)) {
		return 0;
	} else if (0 == strncmp(varname->buf_addr, "$zroutines", varname->len_used)){
		strcpy(ret_value->buf_addr, ".");
		ret_value->len_used = 1;
		return 0;
	}
	boolean_t done = FALSE;
	ydb_buffer_t *t = mock_ptr_type(ydb_buffer_t*);
	YDB_COPY_BUFFER_TO_BUFFER(t, ret_value, done);
	return mock_type(int);
}

int __wrap_ydb_subscript_next_s(ydb_buffer_t *varname, int subs_used, ydb_buffer_t *subsarray, ydb_buffer_t *ret_value) {
	ret_value = mock_type(ydb_buffer_t*);
	return mock_type(int);
}

static void test_valid_input_no_parms(void **state) {
	RoctoSession		session;
	ParameterDescription	*description;
	char			*query = "select * from names;";
	int32_t			dlength = 0;
	int16_t			num_parms = 0;
	boolean_t		done;
	ydb_buffer_t		num_parms_buf, session_id;

	// Initialize buffers
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	YDB_COPY_LITERAL_TO_BUFFER("0", &num_parms_buf, done);
	YDB_LITERAL_TO_BUFFER("1", &session_id);
	session.session_id = &session_id;
	// Exclude type field
	dlength = sizeof(ParameterDescription) - 1 + (sizeof(int32_t) * num_parms);

	will_return(__wrap_ydb_get_s, &num_parms_buf);
	will_return(__wrap_ydb_get_s, YDB_OK);

	description = make_parameter_description(query, &session);

	assert_int_equal(0, description->num_parms);
	assert_int_equal(dlength, ntohl(description->length));

	free(description);
}

static void test_valid_input_one_parm_no_types(void **state) {
	RoctoSession		session;
	ParameterDescription	*description;
	char			*query = "select * from names where id = $1;";
	int32_t			dlength = 0, cur_parm_type = 0;
	int16_t			num_parms = 1;
	boolean_t		done;
	ydb_buffer_t		num_parms_buf, session_id;
	ydb_buffer_t		parm1, type1;

	// Initialize buffers
	YDB_LITERAL_TO_BUFFER("1", &parm1);
	YDB_LITERAL_TO_BUFFER("705", &type1);	// PSQL_TypeOid_unknown == 705
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	YDB_COPY_LITERAL_TO_BUFFER("1", &num_parms_buf, done);
	YDB_LITERAL_TO_BUFFER("1", &session_id);
	session.session_id = &session_id;
	// Exclude type field
	dlength = sizeof(ParameterDescription) - 1 + (sizeof(int32_t) * num_parms);

	will_return(__wrap_ydb_get_s, &num_parms_buf);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_OK);

	description = make_parameter_description(query, &session);

	assert_int_equal(num_parms, ntohs(description->num_parms));
	assert_int_equal(dlength, ntohl(description->length));
	for (cur_parm_type = 0; cur_parm_type < num_parms; cur_parm_type++) {
		assert_int_equal(PSQL_TypeOid_unknown, ntohl(((int32_t*)description->data)[cur_parm_type]));
	}

	free(description);
}

static void test_valid_input_two_parms_no_types(void **state) {
	RoctoSession		session;
	ParameterDescription	*description;
	char			*query = "select * from names where id = $1 and firstname = $2;";
	int32_t			dlength = 0, cur_parm_type = 0;
	int16_t			num_parms = 2;
	boolean_t		done;
	ydb_buffer_t		num_parms_buf, session_id;
	ydb_buffer_t		parm1, parm2, type1;

	// Initialize buffers
	YDB_LITERAL_TO_BUFFER("1", &parm1);
	YDB_LITERAL_TO_BUFFER("2", &parm2);
	YDB_LITERAL_TO_BUFFER("705", &type1);	// PSQL_TypeOid_unknown == 705
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	YDB_COPY_LITERAL_TO_BUFFER("2", &num_parms_buf, done);
	YDB_LITERAL_TO_BUFFER("1", &session_id);
	session.session_id = &session_id;
	// Exclude type field
	dlength = sizeof(ParameterDescription) - 1 + (sizeof(int32_t) * num_parms);

	will_return(__wrap_ydb_get_s, &num_parms_buf);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_OK);

	description = make_parameter_description(query, &session);

	assert_int_equal(num_parms, ntohs(description->num_parms));
	assert_int_equal(dlength, ntohl(description->length));
	for (cur_parm_type = 0; cur_parm_type < num_parms; cur_parm_type++) {
		assert_int_equal(PSQL_TypeOid_unknown, ntohl(((int32_t*)description->data)[cur_parm_type]));
	}

	free(description);
}

static void test_valid_input_three_parms_no_types(void **state) {
	RoctoSession		session;
	ParameterDescription	*description;
	char			*query = "select * from names where id = $1 and firstname = $2 or lastname = $3;";
	int32_t			dlength = 0, cur_parm_type = 0;
	int16_t			num_parms = 3;
	boolean_t		done;
	ydb_buffer_t		num_parms_buf, session_id;
	ydb_buffer_t		parm1, parm2, parm3, type1;

	// Initialize buffers
	YDB_LITERAL_TO_BUFFER("1", &parm1);
	YDB_LITERAL_TO_BUFFER("2", &parm2);
	YDB_LITERAL_TO_BUFFER("3", &parm3);
	YDB_LITERAL_TO_BUFFER("705", &type1);	// PSQL_TypeOid_unknown == 705
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	YDB_COPY_LITERAL_TO_BUFFER("3", &num_parms_buf, done);
	YDB_LITERAL_TO_BUFFER("1", &session_id);
	session.session_id = &session_id;
	// Exclude type field
	dlength = sizeof(ParameterDescription) - 1 + (sizeof(int32_t) * num_parms);

	will_return(__wrap_ydb_get_s, &num_parms_buf);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_OK);

	description = make_parameter_description(query, &session);

	assert_int_equal(num_parms, ntohs(description->num_parms));
	assert_int_equal(dlength, ntohl(description->length));
	for (cur_parm_type = 0; cur_parm_type < num_parms; cur_parm_type++) {
		assert_int_equal(PSQL_TypeOid_unknown, ntohl(((int32_t*)description->data)[cur_parm_type]));
	}

	free(description);
}

static void test_valid_input_one_parm_one_type(void **state) {
	RoctoSession		session;
	ParameterDescription	*description;
	char			*query = "select * from names where id = $1;";
	int32_t			dlength = 0;
	int16_t			num_parms = 1;
	boolean_t		done = FALSE;
	ydb_buffer_t		num_parms_buf, session_id;
	ydb_buffer_t		parm1, type1;

	// Initialize buffers
	YDB_LITERAL_TO_BUFFER("1", &parm1);
	YDB_LITERAL_TO_BUFFER("23", &type1);	// PSQL_TypeOid_int4 == 23
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	YDB_COPY_LITERAL_TO_BUFFER("1", &num_parms_buf, done);
	YDB_LITERAL_TO_BUFFER("1", &session_id);
	session.session_id = &session_id;
	// Exclude type field
	dlength = sizeof(ParameterDescription) - 1 + (sizeof(int32_t) * num_parms);

	will_return(__wrap_ydb_get_s, &num_parms_buf);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_OK);

	description = make_parameter_description(query, &session);

	assert_int_equal(num_parms, ntohs(description->num_parms));
	assert_int_equal(dlength, ntohl(description->length));
	assert_int_equal(PSQL_TypeOid_int4, ntohl(((int32_t*)description->data)[0]));

	free(description);
}

static void test_valid_input_two_parms_one_type(void **state) {
	RoctoSession		session;
	ParameterDescription	*description;
	char			*query = "select * from names where id = $1 and lastname = $2;";
	int32_t			dlength = 0;
	int16_t			num_parms = 2;
	boolean_t		done = FALSE;
	ydb_buffer_t		num_parms_buf, session_id;
	ydb_buffer_t		parm1, parm2, type1, type2;

	// Initialize buffers
	YDB_LITERAL_TO_BUFFER("1", &parm1);
	YDB_LITERAL_TO_BUFFER("2", &parm2);
	YDB_LITERAL_TO_BUFFER("23", &type1);	// PSQL_TypeOid_int4 == 23
	YDB_LITERAL_TO_BUFFER("705", &type2);	// PSQL_TypeOid_unknown == 705
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	YDB_COPY_LITERAL_TO_BUFFER("2", &num_parms_buf, done);
	YDB_LITERAL_TO_BUFFER("1", &session_id);
	session.session_id = &session_id;
	// Exclude type field
	dlength = sizeof(ParameterDescription) - 1 + (sizeof(int32_t) * num_parms);

	will_return(__wrap_ydb_get_s, &num_parms_buf);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type2);
	will_return(__wrap_ydb_get_s, YDB_OK);

	description = make_parameter_description(query, &session);

	assert_int_equal(num_parms, ntohs(description->num_parms));
	assert_int_equal(dlength, ntohl(description->length));
	assert_int_equal(PSQL_TypeOid_int4, ntohl(((int32_t*)description->data)[0]));
	assert_int_equal(PSQL_TypeOid_unknown, ntohl(((int32_t*)description->data)[1]));

	free(description);
}

static void test_valid_input_three_parms_two_types(void **state) {
	RoctoSession		session;
	ParameterDescription	*description;
	char			*query = "select * from names where id = $1 and lastname = $2 or firstname = $3;";
	int32_t			dlength = 0;
	int16_t			num_parms = 3;
	boolean_t		done = FALSE;
	ydb_buffer_t		num_parms_buf, session_id;
	ydb_buffer_t		parm1, parm2, parm3, type1, type2, type3;

	// Initialize buffers
	YDB_LITERAL_TO_BUFFER("1", &parm1);
	YDB_LITERAL_TO_BUFFER("2", &parm2);
	YDB_LITERAL_TO_BUFFER("3", &parm3);
	YDB_LITERAL_TO_BUFFER("23", &type1);	// PSQL_TypeOid_int4 == 23
	YDB_LITERAL_TO_BUFFER("1043", &type2);	// PSQL_TypeOid_varchar == 1043
	YDB_LITERAL_TO_BUFFER("705", &type3);	// PSQL_TypeOid_unknown == 705
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	YDB_COPY_LITERAL_TO_BUFFER("3", &num_parms_buf, done);
	YDB_LITERAL_TO_BUFFER("1", &session_id);
	session.session_id = &session_id;
	// Exclude type field
	dlength = sizeof(ParameterDescription) - 1 + (sizeof(int32_t) * num_parms);

	will_return(__wrap_ydb_get_s, &num_parms_buf);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type2);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type3);
	will_return(__wrap_ydb_get_s, YDB_OK);


	description = make_parameter_description(query, &session);

	assert_int_equal(num_parms, ntohs(description->num_parms));
	assert_int_equal(dlength, ntohl(description->length));
	assert_int_equal(PSQL_TypeOid_int4, ntohl(((int32_t*)description->data)[0]));
	assert_int_equal(PSQL_TypeOid_varchar, ntohl(((int32_t*)description->data)[1]));
	assert_int_equal(PSQL_TypeOid_unknown, ntohl(((int32_t*)description->data)[2]));

	free(description);
}

static void test_num_parms_ydb_get_s_failure(void **state) {
	RoctoSession		session;
	ParameterDescription	*description;
	char			*query = "select * from names where id = $1;";
	int32_t			dlength = 0, cur_parm_type = 0;
	int16_t			num_parms = 1;
	boolean_t		done;
	ydb_buffer_t		num_parms_buf, session_id;
	ydb_buffer_t		parm1, type1;

	// Initialize buffers
	YDB_LITERAL_TO_BUFFER("1", &parm1);
	YDB_LITERAL_TO_BUFFER("705", &type1);	// PSQL_TypeOid_unknown == 705
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	YDB_COPY_LITERAL_TO_BUFFER("1", &num_parms_buf, done);
	YDB_LITERAL_TO_BUFFER("1", &session_id);
	session.session_id = &session_id;
	// Exclude type field
	dlength = sizeof(ParameterDescription) - 1 + (sizeof(int32_t) * num_parms);

	will_return(__wrap_ydb_get_s, &num_parms_buf);
	will_return(__wrap_ydb_get_s, YDB_ERR_LVUNDEF);

	description = make_parameter_description(query, &session);

	assert_null(description);
}

static void test_parm_type_ydb_get_s_failure(void **state) {
	RoctoSession		session;
	ParameterDescription	*description;
	char			*query = "select * from names where id = $1;";
	int32_t			dlength = 0, cur_parm_type = 0;
	int16_t			num_parms = 1;
	boolean_t		done;
	ydb_buffer_t		num_parms_buf, session_id;
	ydb_buffer_t		parm1, type1;

	// Initialize buffers
	YDB_LITERAL_TO_BUFFER("1", &parm1);
	YDB_LITERAL_TO_BUFFER("705", &type1);	// PSQL_TypeOid_unknown == 705
	YDB_MALLOC_BUFFER(&num_parms_buf, INT16_TO_STRING_MAX);
	YDB_COPY_LITERAL_TO_BUFFER("1", &num_parms_buf, done);
	YDB_LITERAL_TO_BUFFER("1", &session_id);
	session.session_id = &session_id;
	// Exclude type field
	dlength = sizeof(ParameterDescription) - 1 + (sizeof(int32_t) * num_parms);

	will_return(__wrap_ydb_get_s, &num_parms_buf);
	will_return(__wrap_ydb_get_s, YDB_OK);
	will_return(__wrap_ydb_get_s, &type1);
	will_return(__wrap_ydb_get_s, YDB_ERR_LVUNDEF);

	description = make_parameter_description(query, &session);

	assert_null(description);
}
int main(void) {
	octo_init(0, NULL);
	const struct CMUnitTest tests[] = {
		cmocka_unit_test(test_valid_input_no_parms),
		cmocka_unit_test(test_valid_input_one_parm_no_types),
		cmocka_unit_test(test_valid_input_two_parms_no_types),
		cmocka_unit_test(test_valid_input_three_parms_no_types),
		cmocka_unit_test(test_valid_input_one_parm_one_type),
		cmocka_unit_test(test_valid_input_two_parms_one_type),
		cmocka_unit_test(test_valid_input_three_parms_two_types),
		cmocka_unit_test(test_num_parms_ydb_get_s_failure),
		cmocka_unit_test(test_parm_type_ydb_get_s_failure),
	};
	return cmocka_run_group_tests(tests, NULL, NULL);
}
