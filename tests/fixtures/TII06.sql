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

-- TII06 : OCTO502 : Test INSERT INTO with fewer columns than target table works

INSERT INTO names SELECT id+6 FROM names ORDER BY id DESC LIMIT 2;
INSERT INTO names(firstname,id) SELECT firstname,id+12 FROM names LIMIT 2;
INSERT INTO names SELECT id+24,NULL,NULL FROM names LIMIT 2;

-- Also test INSERT INTO with comma-separated list of columns specified for the target table
-- Use a fancy query that also has OR usages (to do DNF expansion and create new plans as part of logical plan optimization)
INSERT INTO names(lastname, id) SELECT firstname, id+10 FROM names WHERE id = 4 OR id = 5 UNION VALUES ('Lastname100', 100), ('', 200); -- Also used in TV015 subtest

SELECT * FROM names;
SELECT * FROM names where firstname is NULL;
SELECT * FROM names where lastname is NULL;

