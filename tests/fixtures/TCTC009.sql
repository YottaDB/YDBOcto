#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCTC009 : OCTO582 : Test LP_INSERT_INTO/LP_UPDATE/LP_DELETE/LP_CHECK_CONSTRAINT/LP_UNIQUE_CONSTRAINT logical plan output

create table tmp (id1 integer PRIMARY KEY, id2 integer, UNIQUE(id1, id2), CHECK (id1 > 2));
insert into tmp values (3, 4);
select * from tmp;
update tmp set id1 = 4 where id2 = 4;
select * from tmp;
delete from tmp where id2 = 4;
select * from tmp;
drop table tmp;

