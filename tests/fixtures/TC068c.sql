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

drop table if exists tmp;
create table tmp (id1 integer primary key, id2 integer key num 1, id3 integer, id4 integer extract "$extract(values(""ID3""),1,99)*10") READONLY GLOBAL "^x";
select * from tmp;