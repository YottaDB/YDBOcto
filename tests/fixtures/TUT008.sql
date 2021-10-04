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

-- TUT008 : OCTO579 : Test UTF-8 string handling when ydb_chset=UTF-8

-- This file is an extension of TUT007.sql but replacing ascii characters in queries with UTF-8 characters
--
-- This has queries that contain UTF-8 characters and therefore require ydb_chset to be set to UTF-8
-- in order for these to run fine in Octo.
--
-- While TUT007.sql queries can be executed when ydb_chset is M or UTF-8,
-- TUT008.sql queries can only be run when ydb_chset is UTF-8.

drop table if exists TUT008;

-- Test that VARCHAR(4) allows 4 characters (even if it is more than 4 bytes) to be stored
create table TUT008 (column1 character(4));
insert into TUT008 values ('abcd');
update TUT008 set column1 = 'ＡＢＣＤ';
select * from TUT008;
delete from TUT008;

-- Test that VARCHAR(4) allows character strings less than or equal to 4 chars to be stored without space padding at end
-- Test that UPDATE works with NULL piece values too
insert into TUT008 VALUES (NULL);
insert into TUT008 VALUES ('Ａ');
update TUT008 set column1 = column1 || 'Ｂ';
update TUT008 set column1 = column1 || 'Ｃ';
update TUT008 set column1 = column1 || 'Ｄ';
select * from TUT008;

-- Test that VARCHAR(4) allows 7 character strings to be stored if last 3 characters are spaces
update TUT008 set column1 = 'ＡＢＣＤ   ';
select * from TUT008;
update TUT008 set column1 = 'ＡＢＣ    ';
select * from TUT008;

-- Test that an over length value when typecast to varchar(N) will be truncated to N characters without an error
delete from TUT008;
insert into TUT008 values ('|');
update TUT008 set column1 = column1 || 'ＡＢＣＤ'::varchar(2) || '|';
select * from TUT008;
update TUT008 set column1 = '|ＡＢＣＤ|'::varchar(4);
select * from TUT008;

