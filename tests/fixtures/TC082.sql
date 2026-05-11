#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TC082 : OCTO1107 : ERR_GLOBAL_MISSING_KEY_COLS incorrectly issued with COLUMN globals
DROP TABLE IF EXISTS test;
CREATE TABLE test (
	a integer,
	b integer,
	value varchar PIECE 1,
	value_g varchar GLOBAL "^test(keys(""a""))" PIECE 1, --invalid error issued here
	PRIMARY KEY (a, b)
) GLOBAL "^test(keys(""a""),keys(""b""))"
