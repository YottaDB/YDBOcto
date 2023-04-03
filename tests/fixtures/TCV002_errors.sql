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

CREATE VIEW v (total_col)
  AS
     SELECT id FROM names;

SELECT v.test_col FROM v; -- ERROR
SELECT v.test_col; -- ERROR
DROP VIEW v;
SELECT v.test_col FROM v;
