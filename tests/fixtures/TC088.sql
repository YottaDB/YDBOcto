#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TC088 : OCTO1145 : values() of a DELIM "" column is the whole node, not $PIECE(...,"",n)

-- "code" is DELIM "", i.e. the whole ^disp(id) node IS the column value, so no $PIECE applies.
-- "descr" passes values("code") to a lookup in ^dispxr.
-- Before YDBOcto#1145 that values("code") reference emitted $PIECE($GET(^disp(id)),"",1), which
-- is always "" in M, so the lookup was ^dispxr("") and "descr" came back empty on every row.
DROP TABLE IF EXISTS disptbl;
CREATE TABLE disptbl (
	id integer PRIMARY KEY,
	code varchar DELIM "",
	descr varchar EXTRACT "$GET(^dispxr(values(""code"")))"
) GLOBAL "^disp(keys(""id""))" READONLY;

select * from disptbl;
select id, descr from disptbl where descr = 'Chemistry';
