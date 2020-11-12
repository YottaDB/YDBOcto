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

-- TII05 : OCTO502 : Test ERR_INSERT_INTO_TYPE_MISMATCH and ERR_INSERT_INTO_TOO_MANY_COLUMNS errors

-- Test ERR_INSERT_INTO_TYPE_MISMATCH error
INSERT INTO names SELECT firstname FROM names;
INSERT INTO names SELECT lastname FROM names;
INSERT INTO names SELECT firstname,id+6 FROM names;
INSERT INTO names SELECT firstname,firstname,lastname FROM names;
INSERT INTO names SELECT id+6,id,lastname FROM names;
INSERT INTO names SELECT id+6, firstname, TRUE from names;
INSERT INTO names SELECT (id+6)::BOOLEAN, firstname, lastname from names;
INSERT INTO names SELECT id::BOOLEAN, firstname, lastname from names;

-- Test error ERR_INSERT_INTO_TOO_MANY_COLUMNS error
INSERT INTO names SELECT id,firstname,lastname,id FROM names;
INSERT INTO names SELECT NULL,NULL,NULL,NULL FROM names;

