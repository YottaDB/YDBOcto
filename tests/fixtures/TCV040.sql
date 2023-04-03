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

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380488797
create view v as select 1,*Zero;
create view v1 as select 1 as n1_id,'Zero' as n1_firstname,*'Cool' as n1_firstname;
create view v2 as select 1 as n1_id,*'Zero' as n1_firstname, 'Cool' as n1_firstname;
create view v3 as select 1,'Zero',*Cool;
create view v4 as select 1,*Zero,'Cool';
create view v5 as select 1,*Zero,'Cool' from names;
create view v6 as select 1,*;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380450299
create view v4172 as select id "quote"."quote" from names;
select 1;
