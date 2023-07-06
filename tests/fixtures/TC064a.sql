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

-- TC064 : OCTO633 : Correct text table definitions are generated for DDLs with EXTRACT column and > 2 non-key columns

drop table if exists delimnamescol;
CREATE TABLE delimnamescol (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30) DELIM (13), middleInitial VARCHAR(1), age INTEGER, fullName VARCHAR EXTRACT "$$^FULLNAME(values(""firstname""),values(""lastname""))") GLOBAL "^delimnamescol(keys(""id""))";
select * from delimnamescol;
