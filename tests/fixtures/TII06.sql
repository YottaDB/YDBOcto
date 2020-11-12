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

INSERT INTO names SELECT id+6 FROM names LIMIT 2;
INSERT INTO names SELECT id+12,firstname FROM names LIMIT 2;
INSERT INTO names SELECT id+24,NULL,NULL FROM names LIMIT 2;
SELECT * FROM names where firstname is NULL;

