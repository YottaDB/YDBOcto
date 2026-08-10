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

-- TQO001 : #1141 : Schema-qualified OPERATOR(pg_catalog.op) form is accepted wherever a bare operator is

select * from names where id operator(pg_catalog.=) 3;
select * from names where id operator(pg_catalog.>) 3;
select n1.id,n2.firstname from names n1 inner join names n2 on n1.id operator(pg_catalog.=) n2.id where n2.id operator(pg_catalog.<=) 2;
