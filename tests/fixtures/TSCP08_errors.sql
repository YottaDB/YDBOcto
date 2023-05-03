#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- Error cases

-- Test of ERR_SELECT_STAR_NO_TABLES error
select *;
select 1,*;
select true,'a',*;

-- Test of ERR_MISSING_FROM_ENTRY error
select n1.*;
select 1,n1.*;
select n1.* from names n2;
select * from (select n1.* from names n2);

-- Test of ERR_SELECT_STAR_NO_TABLES error
select * as alias from names;
select * alias from names;

-- Test of ERR_SUBQUERY_ONE_COLUMN error
select (select names.*) from names;

-- ORDER BY TABLENAME.ASTERISK usage with wrong TABLENAME
select * from (select firstname,lastname,id from names)n1 order by n2.*,n1.id;
select id from names n1 order by n2.*;
select n1.* from names n1 order by *,n1.id;
