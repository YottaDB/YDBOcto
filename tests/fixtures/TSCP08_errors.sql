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
-- Below queries should not result in any seg faults
select *;
select n1.*;
select n1.* from names n2;
select * from (select n1.* from names n2);
select * as alias from names;
select * alias from names;
-- order by TABLENAME.ASTERISK usage with wrong TABLENAME
select * from (select firstname,lastname,id from names)n1 order by n2.*,n1.id;
select id from names n1 order by n2.*;
select n1.* from names n1 order by *,n1.id;
