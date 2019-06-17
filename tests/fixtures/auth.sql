#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- CREATE TABLE auth (id INTEGER PRIMARY KEY, userName VARCHAR(30), pwHash VARCHAR(36));
CREATE TABLE users (
    oid INTEGER,
    rolname VARCHAR KEY NUM "0",
		rolsuper INTEGER,
    rolinherit INTEGER,
    rolcreaterole INTEGER,
    rolcreatedb INTEGER,
    rolcanlogin INTEGER,
    rolreplication INTEGER,
    rolbypassrls INTEGER,
    rolconnlimit INTEGER,
    rolpassword VARCHAR,
    rolvaliduntil VARCHAR
)
