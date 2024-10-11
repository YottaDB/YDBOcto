#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

drop table if exists tbt15;
create table tbt15 (id int, foo boolean);
insert into tbt15 values(1, 't');
insert into tbt15 values(2, 'yes');
insert into tbt15 values(3, 'y');
insert into tbt15 values(4, 'true');
insert into tbt15 values(5, 'false');
insert into tbt15 values(6, 'n');
insert into tbt15 values(7, 'no');
insert into tbt15 values(8, 'f');
insert into tbt15 values(9, 'y'), (10, 'n');
insert into tbt15 values(10, 'y'), (11, false);
select * from tbt15;

drop table if exists tbt15;
create table tbt15 (id int, foo boolean);
insert into tbt15 select 1, 't';
insert into tbt15 select 2, 'yes';
insert into tbt15 select 3, 'y';
insert into tbt15 select 4, 'true';
insert into tbt15 select 5, 'false';
insert into tbt15 select 6, 'n';
insert into tbt15 select 7, 'no';
insert into tbt15 select 8, 'f';
insert into tbt15 select 9, 't' union select 11, false;
select * from tbt15;

drop table tbt15;

-- Following query used to throw ERR_INSERT_TYPE_MISMATCH in Octo but is fixed by #1061 changes.
-- It works in Postgres too. It is moved from TBT13 hence the table name.
create table tbt13 (txt boolean);
insert into tbt13 values ('t'), ('f');
select * from tbt13;
drop table tbt13;
