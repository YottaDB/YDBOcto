#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSC21 : Test SELECT COLUMN list values of length ~ 16Kb

create table longvalues (id INTEGER PRIMARY KEY, value VARCHAR) GLOBAL "^longvalues(keys(""id""))";
select id from longvalues order by id;
select id from longvalues where value ~ id::varchar order by id;
select value from longvalues order by id;
select id,value from longvalues order by id;

