#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TP001 : OCTO656 : INSERT, UPDATE, DELETE prohibited if rocto run without -w

-- Test table modification prohibited
insert into TP001 (select * FROM names);
select * from TP001;
update TP001 set firstname = lastname, lastname = firstname where lastname != 'Cool';
select * from TP001;
delete from TP001 where lastname != 'Burn';
select * from TP001;
