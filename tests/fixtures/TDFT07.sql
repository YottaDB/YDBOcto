#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDFT07 : OCTO54 : Test DELETE FROM and INSERT INTO after xref plan files get deleted issues ZLINKFILE error

INSERT INTO names SELECT id+2 FROM names ORDER BY id DESC LIMIT 2;

-- Since all of the below queries are expected to issue ZLINKFILE error, take this opportunity to
-- run couple of DELETE FROM queries, the latter of which exposed an incorrectly coded assert in
-- qualify_column_name.c (which has since been fixed).
DELETE FROM names n1 WHERE n1.id IN (SELECT n2.id FROM names n2 ORDER BY n2.id DESC LIMIT 2);
DELETE FROM names WHERE id IN (SELECT id FROM names ORDER BY id DESC LIMIT 2);

