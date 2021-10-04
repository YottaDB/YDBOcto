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

-- TUT006 : OCTO579 : Test that UPDATE after xref plan files get deleted, issues ZLINKFILE error

UPDATE names n1 SET firstname = firstname || '?' WHERE n1.id IN (SELECT n2.id FROM names n2 ORDER BY n2.id DESC LIMIT 3);
UPDATE names SET firstname = firstname || '#' WHERE id IN (SELECT id FROM names ORDER BY id DESC LIMIT 3);

