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

-- TAF06 : OCTO617 : Test YDBOcto#617 optimization for various edge cases

-- Note: These are queries that use OCTO specific keywords and so are not kept in TAF06.sql (which is used against Postgres too).

-- Test that empty table works
create table TAF06octoa (id integer primary key);
select min(id) is null from TAF06octoa;
select max(id) is null from TAF06octoa;

-- Test that STARTINCLUDE keyword is NOT optimized
create table TAF06octob (id integer primary key START 4 STARTINCLUDE);
select min(id) is null from TAF06octob;
select max(id) is null from TAF06octob;

-- Test that END keyword is NOT optimized
create table TAF06octoc (id integer primary key END "'0");
select min(id) is null from TAF06octoc;
select max(id) is null from TAF06octoc;

-- Test that START and ENDPOINT keywords are optimized for INTEGER type
create table TAF06octod (id integer primary key START 4 ENDPOINT 8) GLOBAL "^TAF06octod";
select min(id) from TAF06octod;
select max(id) from TAF06octod;

-- Test START and ENDPOINT keywords for INTEGER type column that map to a table with NO data
create table TAF06octod2 (id integer primary key START 10 ENDPOINT 12) GLOBAL "^TAF06octod";
select min(id) is null from TAF06octod2;
select max(id) is null from TAF06octod2;

-- Test that START and ENDPOINT keywords are optimized for VARCHAR type
create table TAF06octoe (id varchar primary key START '"d"' ENDPOINT '"h"') GLOBAL "^TAF06octoe";
select min(id) from TAF06octoe;
select max(id) from TAF06octoe;

-- Test START and ENDPOINT keywords for VARCHAR type column that map to a table with NO data
create table TAF06octoe2 (id varchar primary key START '"j"' ENDPOINT '"l"') GLOBAL "^TAF06octoe";
select min(id) is null from TAF06octoe2;
select max(id) is null from TAF06octoe2;

----------------------------------------------------------------------------------------------------
-- Note: The below READWRITE and READONLY tests are done on an empty table more so to verify from
-- the emitted M plan that the optimization is enabled or disabled as appropriate. The actual query
-- results are not checked as it is not considered worth the effort (requires adding data into the tables).
----------------------------------------------------------------------------------------------------
-- Test INTEGER typed key column on READWRITE table
create table TAF06octorw1 (id integer primary key);
select min(id) from TAF06octorw1;
select max(id) from TAF06octorw1;

-- Test NUMERIC typed key column on READWRITE table
create table TAF06octorw2 (id numeric primary key);
select min(id) from TAF06octorw2;
select max(id) from TAF06octorw2;

-- Test VARCHAR typed key column on READWRITE table
create table TAF06octorw3 (id varchar primary key);
select min(id) from TAF06octorw3;
select max(id) from TAF06octorw3;

-- Test BOOLEAN typed key column on READWRITE table
create table TAF06octorw4 (id boolean primary key);
select min(id) from TAF06octorw4;
select max(id) from TAF06octorw4;

-- Test DATE typed key column on READWRITE table
create table TAF06octorw5 (id date primary key);
select min(id) from TAF06octorw5;
select max(id) from TAF06octorw5;

-- Test TIME typed key column on READWRITE table
create table TAF06octorw6 (id time primary key);
select min(id) from TAF06octorw6;
select max(id) from TAF06octorw6;

-- Test TIME WITH TIME ZONE typed key column on READWRITE table
create table TAF06octorw7 (id time with time zone primary key);
select min(id) from TAF06octorw7;
select max(id) from TAF06octorw7;

-- Test TIMESTAMP typed key column on READWRITE table
create table TAF06octorw8 (id timestamp primary key);
select min(id) from TAF06octorw8;
select max(id) from TAF06octorw8;

-- Test TIMESTAMP WITH TIME ZONE typed key column on READWRITE table
create table TAF06octorw9 (id timestamp with time zone primary key);
select min(id) from TAF06octorw9;
select max(id) from TAF06octorw9;

-- Test INTEGER typed key column on READONLY table
create table TAF06octoro1 (id integer primary key) READONLY;
select min(id) from TAF06octoro1;
select max(id) from TAF06octoro1;

-- Test NUMERIC typed key column on READONLY table
create table TAF06octoro2 (id numeric primary key) READONLY;
select min(id) from TAF06octoro2;
select max(id) from TAF06octoro2;

-- Test VARCHAR typed key column on READONLY table
create table TAF06octoro3 (id varchar primary key) READONLY;
select min(id) from TAF06octoro3;
select max(id) from TAF06octoro3;

-- Test BOOLEAN typed key column on READONLY table
create table TAF06octoro4 (id boolean primary key) READONLY;
select min(id) from TAF06octoro4;
select max(id) from TAF06octoro4;

-- Test DATE typed key column on READONLY table
create table TAF06octoro5 (id date primary key) READONLY;
select min(id) from TAF06octoro5;
select max(id) from TAF06octoro5;

-- Test TIME typed key column on READONLY table
create table TAF06octoro6 (id time primary key) READONLY;
select min(id) from TAF06octoro6;
select max(id) from TAF06octoro6;

-- Test TIME WITH TIME ZONE typed key column on READONLY table
create table TAF06octoro7 (id time with time zone primary key) READONLY;
select min(id) from TAF06octoro7;
select max(id) from TAF06octoro7;

-- Test TIMESTAMP typed key column on READONLY table
create table TAF06octoro8 (id timestamp primary key) READONLY;
select min(id) from TAF06octoro8;
select max(id) from TAF06octoro8;

-- Test TIMESTAMP WITH TIME ZONE typed key column on READONLY table
create table TAF06octoro9 (id timestamp with time zone primary key) READONLY;
select min(id) from TAF06octoro9;
select max(id) from TAF06octoro9;

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/617#note_2151071683
-- The below query expects an error and hence is not in TAF06.sql (which is run using crosscheck)
select 1 from names n1 order by (select max(n1.id) from names n2);

