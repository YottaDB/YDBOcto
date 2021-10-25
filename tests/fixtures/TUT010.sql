#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TUT010 : OCTO579 : Crosscheck of simple UPDATE queries in composite database between Octo and Postgres

-- Test sub-query in SET clause works
update TUT010 set name = (select name || '#' from TUT010 ORDER BY 1 DESC limit 1) where name = 'Name7';
select * from TUT010;

-- Test OR operator in WHERE clause works (tests DNF plan expansion logic)
update TUT010 set name = name || '?' where name = 'Name4' or id4 = 5;
select * from TUT010;
update TUT010 set name = name || '@' where name = 'Name10?' or id6 = 6;
select * from TUT010;

-- Test that UPDATE works where column value is specified as NULL
update TUT010 set name = NULL where id7 = 8;
select * from TUT010;
select * from TUT010 where name is NULL;

-- Test UPDATE where 1 non primary key column is modified.
-- Note: This is already tested by the above sections.

-- Test that UPDATE works when primary key column is modified in increasing and decreasing direction
-- a) Also test UPDATE with modifying only a key column
update TUT010 set id1 = id1 + 10, id5 = id5 + 1, id7 = id7 + 4;
select * from TUT010;

-- b) Also test UPDATE with modifying a mix of key and non-key columns at the same time
update TUT010 set id3 = id3 + 3, name = name || '#';
update TUT010 set id3 = id3 + 3, name = name || '#';
update TUT010 set id5 = id5 + 3, id0 = id0 + 9, id7 = id2 + 3, id2 = id7 + 3, name = name || '#';
update TUT010 set id5 = id5 - 3, id0 = id0 - 9, id7 = id2 - 3, id2 = id7 - 3, name = name || '#';
select * from TUT010;

-- Test that UPDATE does not issue a false ERR_DUPLICATE_KEY_VALUE error if key column is modified to same value
update TUT010 set id3 = id3 * 1, id7 = id7, id0 = id0 + 0;

-- Test UPDATE does not issue ZYSQLNULLNOTVALID error (happened in an interim version of the code) when non-key
-- columns are set to a NULL value (see https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/579#note_713264461)
drop table if exists TUT010;
create table TUT010 (id integer primary key, firstname varchar, lastname varchar);
insert into TUT010 values (1, 'f1', 'l1'), (2, 'f2', 'l2'), (3, 'f3', 'l3'), (4, 'f4', 'l4'), (5, 'f5', 'l5');
update TUT010 set firstname = NULL where id < 3;
update TUT010 set lastname = NULL where id = 4;
update TUT010 set firstname = NULL, lastname = NULL where id > 3;
select * from TUT010;

