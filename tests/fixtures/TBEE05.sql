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

-- TBEE05 : OCTO579 : Incorrect results from SELECT/UPDATE/DELETE if prior queries with OR conditions errored out

select '
 -- Test https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/579#note_1105838798
 -- Expect first and third select to output 2 rows and second select to issue ERR_SUBQUERY_MULTIPLE_ROWS error';
select * from names where firstname = 'Joey' or firstname = (select firstname from names where firstname = 'Acid');
select * from names where firstname = 'Joey' or firstname = (select firstname from names where firstname = 'Zero');
select * from names where firstname = 'Joey' or firstname = (select firstname from names where firstname = 'Acid');

select '
 -- Test https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/579#note_1102236999
 -- Expect both update commands to issue ERR_DUPLICATE_KEY_VALUE error';
create table tmp (id1 integer, id2 integer, id3 integer, id4 integer primary key);
insert into tmp values (5, 6, 0, 3);
insert into tmp values (1, 7, 5, 5);
update tmp set id2 = 4, id3 = 2, id4 = 1 where (id3 != 0) and (id1 in (1)) or (id1 != 7);
update tmp set id1 = 6, id2 = 1, id4 = 0 where (id4 < 7) or (id4 in (1, 2, 3)) and (id3 in (1, 2));
drop table tmp;

select '
 -- Test https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/579#note_1100898908
 -- Expect both delete commands to report DELETE 2';
create table tmp (id1 integer primary key);
insert into tmp values (6);
insert into tmp values (0);
delete from tmp where (id1 != 3) or (id1 > 0);
select * from tmp;
delete from tmp;
insert into tmp values (6);
insert into tmp values (0);
update tmp set id1 = 2 where (id1 = 4) or (id1 != 5);
select * from tmp;
delete from tmp where (id1 != 3) or (id1 > 0);
select * from tmp;
drop table tmp;

