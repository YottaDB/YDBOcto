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

-- TQO004 : #1141 : The OPERATOR() construct is case and whitespace insensitive

select id from names where id OPERATOR(PG_CATALOG.=) 3;
select id from names where id OpErAtOr(Pg_CaTaLoG.>=) 4;
select id from names where id operator ( pg_catalog . <= ) 1;
select id from names where id operator(
	pg_catalog.<>
) 3;
