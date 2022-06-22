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

-- TC068 : OCTO633 : EXTRACT containing values() works after manual additional of rows to table

drop table if exists tmp;
create table tmp (id1 integer primary key, id2 integer, id3 integer extract "$extract(values(""ID2""),1,99)*10") READONLY GLOBAL "^x";
select * from tmp;
