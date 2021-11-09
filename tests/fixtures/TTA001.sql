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

-- TTA001 : OCTO759 : COUNT(DISTINCT TABLENAME.ASTERISK) produces incorrect results in some cases : nullcharnames database

-- Below is the query that failed the pipeline (as noted at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_728237002)
SELECT nullcharnames.id,COUNT(alias1.*) FROM nullcharnames  CROSS JOIN (SELECT ALL alias1.firstName FROM nullcharnames alias1 ORDER BY alias1.firstName) AS alias1 WHERE nullcharnames.id IN (4, 1) GROUP BY alias1.*, nullcharnames.id HAVING COUNT(DISTINCT alias1.*) BETWEEN 0 AND 0 ORDER BY COUNT(alias1.*), nullcharnames.id;

