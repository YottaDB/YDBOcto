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

-- This script is sourced by the following subtests

-- TP001 : OCTO656/OCTO609 : INSERT, UPDATE, DELETE, TRUNCATE prohibited if rocto run without -w
-- TP002 : OCTO656/OCTO609 : INSERT, UPDATE, DELETE, TRUNCATE prohibited if rocto run with -w but user lacks readwrite permissions
-- TP003 : OCTO656 : All permissions granted if rocto run with -aw and user has readwrite+allowschemachanges permissions
-- TP006 : OCTO656/OCTO609 : INSERT, UPDATE, DELETE, TRUNCATE prohibited if user has allowschemachanges permissions only (lacks readwrite)

-- Test table modification prohibited
insert into TP001 (select * FROM names);
select * from TP001;
update TP001 set firstname = lastname, lastname = firstname where lastname != 'Cool';
select * from TP001;
delete from TP001 where lastname != 'Burn';
select * from TP001;
truncate TP001;
select * from TP001;
