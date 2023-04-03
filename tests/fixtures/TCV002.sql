#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE VIEW TCV002_1v (test_col)
  AS
     SELECT id FROM names;
SELECT test_col FROM TCV002_1v;
SELECT TCV002_1v.test_col FROM TCV002_1v;
SELECT TCV002_1v.* FROM TCV002_1v;
SELECT * FROM TCV002_1v;
DROP VIEW TCV002_1v;
