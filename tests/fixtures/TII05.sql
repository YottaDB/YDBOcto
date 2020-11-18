#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TII05 : OCTO502 : Test various errors in INSERT INTO

-- Test ERR_INSERT_TYPE_MISMATCH error
INSERT INTO names SELECT firstname FROM names;
INSERT INTO names SELECT lastname FROM names;
INSERT INTO names SELECT firstname,id+6 FROM names;
INSERT INTO names SELECT firstname,firstname,lastname FROM names;
INSERT INTO names SELECT id+6,id,lastname FROM names;
INSERT INTO names SELECT id+6,firstname,TRUE from names;
INSERT INTO names SELECT (id+6)::BOOLEAN,firstname,lastname from names;
INSERT INTO names SELECT id::BOOLEAN,firstname,lastname from names;
INSERT INTO names(firstname,id) SELECT id,firstname FROM names;
INSERT INTO names(firstname,id,lastname) SELECT * FROM names;

-- Test ERR_INSERT_TOO_MANY_EXPRESSIONS error
INSERT INTO names SELECT id,firstname,lastname,id FROM names;
INSERT INTO names SELECT NULL,NULL,NULL,NULL FROM names;
INSERT INTO names(id) SELECT id,firstname FROM names;
INSERT INTO names(id) SELECT id,firstname FROM names;
INSERT INTO names(id,firstname) SELECT id,firstname,id FROM names;

-- Test ERR_INSERT_TOO_MANY_COLUMNS error
INSERT INTO names(id,firstname) SELECT id FROM names;

-- Test ERR_TABLE_UNKNOWN_COLUMN_NAME error
INSERT INTO names(invalid) SELECT * FROM names;
INSERT INTO names(firstname,invalid) SELECT * FROM names;
INSERT INTO names(id,lastname,invalid) SELECT * FROM names;

-- Test that only ERR_TABLE_UNKNOWN_COLUMN_NAME error is issued if ERR_DUPLICATE_COLUMN error also exists
INSERT INTO names(invalid,invalid) SELECT * FROM names;

-- Test ERR_DUPLICATE_COLUMN error
INSERT INTO names(id,invalid,id) SELECT * FROM names;
INSERT INTO names(id,firstname,firstname,lastname) SELECT * FROM names;
INSERT INTO names(id,firstname,firstname,firstname,lastname) SELECT * FROM names;

