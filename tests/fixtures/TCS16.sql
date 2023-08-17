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

-- TCS16 : OCTO1009 : CASE should not evaluate ALL branch values as it can cause errors (e.g. ZYSQLNULLNOTVALID)

-- Test https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/1009#note_1501103697
-- Without the OCTO1009 fixes, this would issue a ZYSQLNULLNOTVALID error.
drop function if exists datetime(varchar);
create function datetime(varchar) returns varchar as $$^TCS16datetime;
select id, case when lastname is null then 'active' else datetime(lastname) end as "inactive_date" from names;

-- Test https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/1009#note_1517211478
-- Without the OCTO1009 fixes, this would issue a "divide by zero" error.
select case when lastname is null then 1 else id / 0 end from names where lastname is null;

