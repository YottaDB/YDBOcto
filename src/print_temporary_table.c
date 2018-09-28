#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <libyottadb.h>

#include "octo.h"
#include "octo_types.h"

void print_temporary_table(SqlTable *table) {
	int status = 0, done = 0;
	char *first_char;
	SqlValue *tableName;
	ydb_buffer_t schema_global, rowid, row_value, empty_buffer;
	ydb_buffer_t z_status, z_status_value;

	INIT_YDB_BUFFER(&schema_global, MAX_STR_CONST);
	UNPACK_SQL_STATEMENT(tableName, table->tableName, value);
	*schema_global.buf_addr++ = '^';
	YDB_COPY_STRING_TO_BUFFER(tableName->v.reference, &schema_global, done);
	schema_global.buf_addr--;
	schema_global.len_used++;
	INIT_YDB_BUFFER(&rowid, MAX_STR_CONST);
	INIT_YDB_BUFFER(&row_value, MAX_STR_CONST);
	YDB_LITERAL_TO_BUFFER("", &empty_buffer);

	status = ydb_subscript_next_s(&schema_global, 1, &rowid, &rowid);
	YDB_ERROR_CHECK(status, &z_status, &z_status_value);

	while(!YDB_BUFFER_IS_SAME(&empty_buffer, &rowid)) {
		status = ydb_get_s(&schema_global, 1, &rowid, &row_value);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
		row_value.buf_addr[row_value.len_used] = '\0';
		first_char = row_value.buf_addr;
		while(first_char < row_value.buf_addr + row_value.len_used && *first_char++ != '|') {
			// Intentionally left blank
		}
		fprintf(stdout, "|%s\n", first_char);
		status = ydb_subscript_next_s(&schema_global, 1, &rowid, &rowid);
		YDB_ERROR_CHECK(status, &z_status, &z_status_value);
	}

	free(row_value.buf_addr);
}
