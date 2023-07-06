#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC066 : OCTO633 : EXTRACT containing values() expression works when there are single or multiple key columns

drop table if exists tmp;
create table tmp (id1 integer primary key, id2 integer, id3 integer extract "$extract(values(""id2""),1,99)*10") READONLY GLOBAL "^x";
select id1 from tmp;
select id2 from tmp;
select id3 from tmp;

drop table if exists tmp;
create table tmp (id1 integer primary key, id2 integer key num 1, id3 integer, id4 integer extract "$extract(values(""id3""),1,99)*10") READONLY GLOBAL "^x";
select id1 from tmp;
select id2 from tmp;
select id3 from tmp;
select id4 from tmp;
