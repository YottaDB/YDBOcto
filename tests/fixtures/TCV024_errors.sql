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

-- TCV024 : Verify that INSERT generates an error when applied on views
create view v1 as select id from names;
insert into v1 values(99); -- ERR_VIEW_OPERATION_NOT_SUPPORTED error
select * from v1; -- Expect no row with id 99
select * from names; -- Expect no change

update v1 set id=666; -- ERR_VIEW_OPERATION_NOT_SUPPORTED error
select * from v1; -- Expect no rows are updated with id = 666
select * from names; -- Expect no change

delete from v1 where id=3; -- ERR_VIEW_OPERATION_NOT_SUPPORTED error
select * from v1; -- Expect no row change
select * from names; -- Expect no change

delete from v1; -- ERR_VIEW_OPERATION_NOT_SUPPORTED error
select * from v1; -- Expect no row change
select * from names; -- Expect no change

-- Test that the INSERT statements below result in an ERR_VIEW_OPERATION_NOT_SUPPORTED error
DROP VIEW IF EXISTS v1;
CREATE VIEW v1 AS select id,lastname as firstname,firstname as lastname from names;
select * from v1;
insert into v1 values (1,2,3);
insert into v1 values (1,'abcd','efgh');

