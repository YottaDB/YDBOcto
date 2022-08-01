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

-- TPBI002 : OCTO867 : Subqueries are accepted in table join list (PowerBI-style syntax)
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1236#note_1316982961 for more background.

select * from ((select * from names));
select * from ((select * from names) inner join names on TRUE);
select * from ((select * from names) ref inner join names on TRUE);
