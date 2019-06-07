#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

add_test(testCreateTables octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/src/samples/create_table.sql)
add_test(testDeleteValues octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/src/samples/delete.sql)
add_test(testSelectValues octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/src/samples/select.sql)
add_test(testInsertValues octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/src/samples/insert.sql)
add_test(testUpdateValues octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/src/samples/update.sql)
add_test(testDropTables octo --dry-run --input-file ${PROJECT_SOURCE_DIR}/src/samples/drop_table.sql)
