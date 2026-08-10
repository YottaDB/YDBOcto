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

-- TQO006 : #1141 : Qualified operators work in the WHERE, ON and HAVING clauses

select id from names where id operator(pg_catalog.>=) 2;
select n1.id from names n1 inner join names n2 on n1.id operator(pg_catalog.=) n2.id and n2.id operator(pg_catalog.<) 3;
select n1.id from names n1 left outer join names n2 on n1.id operator(pg_catalog.<) n2.id;
select lastname from names group by lastname having count(*) operator(pg_catalog.>) 1;
select lastname from names group by lastname having min(id) operator(pg_catalog.<=) 2;
