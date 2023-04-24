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

-- TC075 : OCTO918 : Check EXTRACT and GLOBAL keyword for valid column references at CREATE TABLE time

-- Test of ERR_TABLE_MUST_HAVE_A_NON_EXTRACT_COLUMN error
-- Below query is pasted from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/918#note_1149782871
CREATE TABLE tmp (fullname VARCHAR EXTRACT "$$^FULLNAME(values(""firstName""))") READONLY;

-- Below query is similar to the above one except we have more than 1 EXTRACT column
CREATE TABLE tmp (fullname VARCHAR EXTRACT "$$^FULLNAME", fullname2 VARCHAR EXTRACT "$$^FULLNAME") READONLY;

-- Test of ERR_UNKNOWN_COLUMN_NAME error
CREATE TABLE tmp (id INTEGER PRIMARY KEY, fullname VARCHAR EXTRACT "$$^FULLNAME(values(""fullName""))") READONLY;
CREATE TABLE tmp (id INTEGER PRIMARY KEY, fullname VARCHAR EXTRACT "$$^FULLNAME(keys(""fullName""))") READONLY;
CREATE TABLE tmp (fullname VARCHAR EXTRACT "$$^FULLNAME(values(""invalidColumn""))") READWRITE;
CREATE TABLE tmp (fullname VARCHAR EXTRACT "$$^FULLNAME(keys(""invalidColumn""))") READWRITE;
-- Below queries are from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/918#description
CREATE TABLE `TMP` (`ID` INTEGER PRIMARY KEY, `FULLNAME` VARCHAR EXTRACT "$$^FULLNAME(keys(""firstName""))");
CREATE TABLE `TMP` (`ID` INTEGER PRIMARY KEY, `FULLNAME` VARCHAR EXTRACT "$$^FULLNAME(values(""firstName""))");

-- Test of ERR_VALUES_NEEDS_A_NON_KEY_COLUMN error
-- Todo tracked at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1246#note_1150215340
-- Test EXTRACT with values(primary-key-column)
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), abs_id INTEGER EXTRACT "$$ABS^%ydboctosqlfunctions(values(""ID""))") GLOBAL "^names";

-- Test of ERR_KEYS_NEEDS_A_KEY_COLUMN error
-- Todo tracked at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1246#note_1150217632
-- Test EXTRACT with keys(non-primary-key-column)
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30), computed VARCHAR EXTRACT "$$PostgreSQL^%ydboctofCONCAT(keys(""ID""),values(""FIRSTNAME""),keys(""LASTNAME""))") GLOBAL "^names";

-- ###############################################################################################
-- Test TABLE-LEVEL GLOBAL keyword for various keys() and values() usages.
-- Todo tracked at https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/918#note_1331738086
-- ###############################################################################################

-- Test GLOBAL keyword with keys(non-existent-column)
-- Test of ERR_UNKNOWN_COLUMN_NAME error
create table tmp (id integer primary key) GLOBAL "^names(keys(""firstName"")))" READONLY;
create table tmp (id integer primary key) GLOBAL "^names(keys(""firstName"")))" READWRITE;
create table tmp (id integer primary key, comp varchar EXTRACT "$$abcd") GLOBAL "^names(keys(""firstName"")))" READONLY;
-- Test GLOBAL keyword with values(non-existent-column)
-- Test of ERR_UNKNOWN_COLUMN_NAME error
create table tmp (id integer primary key) GLOBAL "^names(values(""firstName"")))" READONLY;
create table tmp (id integer primary key) GLOBAL "^names(values(""firstName"")))" READWRITE;
create table tmp (id integer primary key, comp varchar EXTRACT "$$abcd") GLOBAL "^names(values(""firstName"")))" READONLY;

-- Test GLOBAL keyword with keys(non-key-column)
-- Test of ERR_KEYS_NEEDS_A_KEY_COLUMN error
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(keys(""FIRSTNAME"")))" READONLY;
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(keys(""FIRSTNAME"")))" READWRITE;
create table tmp (id integer primary key, firstname varchar, comp varchar EXTRACT "$$abcd") GLOBAL "^names(keys(""FIRSTNAME"")))" READONLY;

-- Test GLOBAL keyword with values(key-column)
-- Test of ERR_VALUES_NOT_ALLOWED_IN_GLOBAL error
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(values(""ID"")))" READONLY;
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(values(""ID"")))" READWRITE;
create table tmp (id integer primary key, firstname varchar, comp varchar EXTRACT "$$abcd") GLOBAL "^names(values(""ID"")))" READONLY;

-- Test GLOBAL keyword with values(non-key-column)
-- Test of ERR_VALUES_NOT_ALLOWED_IN_GLOBAL error
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(values(""FIRSTNAME"")))" READONLY;
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(values(""FIRSTNAME"")))" READWRITE;
create table tmp (id integer primary key, firstname varchar, comp varchar EXTRACT "$$abcd") GLOBAL "^names(values(""FIRSTNAME"")))" READONLY;

-- Test GLOBAL keyword with keys(key-column) where key-column is not correct case
-- Test of ERR_UNKNOWN_COLUMN_NAME error
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(keys(""id"")))" READONLY;
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(keys(""id"")))" READWRITE;
create table tmp (id integer primary key, firstname varchar, comp varchar EXTRACT "$$abcd") GLOBAL "^names(keys(""id"")))" READONLY;

-- Test GLOBAL keyword with subscripts specified but NO keys(key-column) specified
-- Test of ERR_GLOBAL_MISSING_KEY_COLS error
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(1)" READONLY;
create table tmp (id integer primary key, firstname varchar) GLOBAL "^names(1)" READWRITE;
create table tmp (id integer primary key, firstname varchar, comp varchar EXTRACT "$$abcd") GLOBAL "^names(1)" READONLY;

-- Test GLOBAL keyword with subscripts specified but only a SUBSET of keys(key-column)s specified in the case of a composite key
-- Test of ERR_GLOBAL_MISSING_KEY_COLS error
create table tmp (id1 integer, id2 integer, primary key (id1, id2), firstname varchar) GLOBAL "^names(1,keys(""ID1""))" READONLY;
create table tmp (id1 integer, id2 integer, primary key (id1, id2), firstname varchar) GLOBAL "^names(1,keys(""ID1""))" READWRITE;
create table tmp (id1 integer, id2 integer, primary key (id1, id2), firstname varchar, comp varchar EXTRACT "$$abcd") GLOBAL "^names(1,keys(""ID1""))" READONLY;

-- Test GLOBAL keyword with subscripts specified where keys(key-column) order is not the same as the KEY NUM order
-- Test of ERR_GLOBAL_KEY_COLS_ORDER error
create table tmp (id1 integer, id2 integer, primary key (id1, id2), firstname varchar) GLOBAL "^names(1,keys(""ID2""),2,keys(""ID1""))" READONLY;
create table tmp (id1 integer, id2 integer, primary key (id1, id2), firstname varchar) GLOBAL "^names(1,keys(""ID2""),2,keys(""ID1""))" READWRITE;
create table tmp (id1 integer, id2 integer, primary key (id1, id2), firstname varchar, comp varchar EXTRACT "$$abcd") GLOBAL "^names(1,keys(""ID2""),2,keys(""ID1""))" READONLY;

-- ###############################################################################################
-- Test GLOBAL keyword for invalid keys() usages does not cause assert failure.
-- ###############################################################################################
-- Some of the below queries work fine. Some of them generate errors. Some errors show up at CREATE TABLE time and some errors
-- show up at SELECT time (after the M code has been emitted). This is considered acceptable for now.
-- Previously some of these could assert fail at SELECT time which is not considered user-friendly.

-- Test of ERR_UNKNOWN_COLUMN_NAME error
drop table tmp;
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, PRIMARY KEY (id2, id1)) GLOBAL "^x(keys(""ID1"",keys(""ID2"")))" READONLY;
select * from tmp;

drop table tmp;
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, PRIMARY KEY (id2, id1)) GLOBAL "^x(keys(ID1))" READONLY;
select * from tmp;

drop table tmp;
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, PRIMARY KEY (id2, id1)) GLOBAL "^x(keys(""""ID1))" READONLY;
select * from tmp;

drop table tmp;
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, PRIMARY KEY (id2, id1)) GLOBAL "^x(keys(""""ID1)" READONLY;
select * from tmp;

-- Test of ERR_GLOBAL_MISSING_KEY_COLS error
drop table tmp;
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, PRIMARY KEY (id1, id2)) GLOBAL "^x(keys(""ID1""),keys(""ID2" READONLY;
select * from tmp;

drop table tmp;
CREATE TABLE TMP (id1 INTEGER PRIMARY KEY) GLOBAL "^x(keys(""ID1"")" READONLY;
select * from tmp;

drop table tmp;
CREATE TABLE TMP (id1 INTEGER PRIMARY KEY, firstname varchar) GLOBAL "^x(keys(""ID1"")" READONLY;
select * from tmp;

drop table tmp;
CREATE TABLE TMP (id1 INTEGER PRIMARY KEY) GLOBAL "^x(keys(""ID1"")x" READONLY;
select * from tmp;

drop table tmp;
CREATE TABLE TMP (id1 INTEGER PRIMARY KEY, firstname varchar) GLOBAL "^x(keys(""ID1"")x" READONLY;
select * from tmp;

drop table tmp;
CREATE TABLE TMP (id1 INTEGER PRIMARY KEY) GLOBAL "^x(keys(""ID1"")xy" READONLY;
select * from tmp;

drop table tmp;
CREATE TABLE TMP (id1 INTEGER PRIMARY KEY, firstname varchar) GLOBAL "^x(keys(""ID1"")xy)" READONLY;
select * from tmp;

-- ###############################################################################################
-- Test COLUMN-LEVEL GLOBAL keyword gets validated for keys() usages
-- ###############################################################################################

-- Test ERR_UNKNOWN_COLUMN_NAME error
-- Below query is pasted from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1363#note_1367080846
CREATE TABLE tmp (
        id       INTEGER,
        given    VARCHAR(15),
        surname  VARCHAR(15),
        street1  VARCHAR(50) GLOBAL "^names(keys(""id""),""Address"")" DELIM '^' PIECE 1,
        street2  VARCHAR(50) GLOBAL "^names(keys(""id""),""Address"")" DELIM '^' PIECE 2,
        city     VARCHAR(30) GLOBAL "^names(keys(""id""),""Address"")" DELIM '^' PIECE 3,
        province VARCHAR(30) GLOBAL "^names(keys(""id""),""Address"")" DELIM '^' PIECE 4,
        country  VARCHAR(30) GLOBAL "^names(keys(""id""),""Address"")" DELIM '^' PIECE 5,
        postal   VARCHAR(10) GLOBAL "^names(keys(""id""),""Address"")" DELIM '^' PIECE 6,
        primary key (id)
) GLOBAL "^names";

-- Below queries are similar to those tested in the TABLE-LEVEL GLOBAL keyword section above.
-- Test of ERR_KEYS_NEEDS_A_KEY_COLUMN error
create table tmp (id integer primary key, firstname varchar GLOBAL "^names(keys(""FIRSTNAME"")))") READONLY;

-- Test of ERR_VALUES_NOT_ALLOWED_IN_GLOBAL error
create table tmp (id integer primary key, firstname varchar GLOBAL "^names(values(""FIRSTNAME"")))") READONLY;
create table tmp (id integer primary key, firstname varchar GLOBAL "^names(values(""ID"")))") READONLY;

-- Test of ERR_GLOBAL_MISSING_KEY_COLS error
create table tmp (id integer primary key, firstname varchar GLOBAL "^names(1)") READONLY;
create table tmp (id1 integer, id2 integer, primary key (id1, id2), firstname varchar GLOBAL "^names(1,keys(""ID1""))") READONLY;

-- Test of ERR_GLOBAL_KEY_COLS_ORDER error
create table tmp (id1 integer, id2 integer, primary key (id1, id2), firstname varchar GLOBAL "^names(1,keys(""ID2""),2,keys(""ID1""))") READONLY;

