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

