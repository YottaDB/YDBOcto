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

create table test1(id integer primary key, foo boolean) global "^test1" readonly;
create table test2(foo boolean primary key, id integer) global "^test2" readonly;
select * from test1 where foo=true;
select * from test1 where foo='no';
select * from test1 where foo in (true,false);
select * from test2 where foo=true;
select * from test2 where foo='no';
select * from test2 where foo in (true,false);
