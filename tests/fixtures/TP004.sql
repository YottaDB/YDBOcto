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

-- TP004 : OCTO656 : CREATE, DROP prohibited if rocto run without -a

-- Schema changes prohibited (no allowschemachanges)
drop table if exists TP004;
create table TP004 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30));
-- Table modifications allowed (readwrite)
insert into names values (6, 'Mr.', 'Numbers');
select * from names;
update names set firstname = lastname, lastname = firstname where lastname != 'Cool';
select * from names;
delete from names where lastname != 'Mr.';
select * from names;
