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

-- TQO002 : #1141 : All six comparison operators are accepted inside OPERATOR(pg_catalog.op)

select id from names where id operator(pg_catalog.=) 3;
select id from names where id operator(pg_catalog.<>) 3;
select id from names where id operator(pg_catalog.!=) 3;
select id from names where id operator(pg_catalog.<) 3;
select id from names where id operator(pg_catalog.>) 3;
select id from names where id operator(pg_catalog.<=) 3;
select id from names where id operator(pg_catalog.>=) 3;
