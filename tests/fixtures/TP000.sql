#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TP000 : OCTO656/OCTO609 : INSERT, UPDATE, DELETE, TRUNCATE prohibited when: table is READONLY, rocto run with -w, and user can readwrite

insert into TP000 (select * FROM names);
select * from TP000;
update TP000 set firstname = lastname, lastname = firstname where lastname != 'Cool';
select * from TP000;
delete from TP000 where lastname != 'Burn';
select * from TP000;
truncate TP000;
select * from TP000;
