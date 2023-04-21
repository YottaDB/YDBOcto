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

