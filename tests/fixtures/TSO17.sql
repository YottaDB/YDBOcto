#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSO17 : OCTO969 : Test SET operations query result table has the column names from first set operation instead of the last
-- Both of the below queries are expected to have a result table with colum name `y`
select * from ((select id as y from names) union (select 1+id from names) union (select 1+id as x from names)) n1;
((select id as y from names) union (select 1+id from names) union (select 1+id as x from names));
