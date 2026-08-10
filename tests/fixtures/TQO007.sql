#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TQO007 : #1141 : Qualified operators work with the ANY/ALL/SOME quantifiers

select id from names where id operator(pg_catalog.=) any (select id from names where id > 3);
select id from names where id operator(pg_catalog.<) all (select id from names where id > 3);
select id from names where id operator(pg_catalog.>=) some (select id from names where id < 2);
select id from names where id operator(pg_catalog.<>) all (select id from names where id >= 4);
select id from names where id operator(=) any (select id from names where id <= 1);
