#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TX07a : This implements the test at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/740#description
-- This tests that IS NULL related xrefs are maintained by triggers even after the initial scan.

create table tbl (id integer primary key, firstname varchar, lastname varchar);
insert into tbl values (1, 'a1', 'b1');
insert into tbl values (2, 'a2', '');
insert into tbl values (3, '', 'b3');
select * from tbl where firstname IS NULL;
select * from tbl where lastname IS NULL;
insert into tbl values (4, '', '');
select '# Expect row with primary key 4 to show up in the two sets of output below';
select * from tbl where firstname IS NULL;
select * from tbl where lastname IS NULL;
drop table tbl;

