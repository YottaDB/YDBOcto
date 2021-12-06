#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TP005 : OCTO656 : CREATE, DROP prohibited if rocto run with -a, user lacks allowschemachanges permissions

-- Schema changes prohibited (no allowschemachanges)
drop table if exists TP005;
create table TP005 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30));
-- Table modifications prohibited (noreadwrite)
insert into names (select * FROM names);
select * from names;
update names set firstname = lastname, lastname = firstname where lastname != 'Cool';
select * from names;
delete from names where lastname != 'Burn';
select * from names;
