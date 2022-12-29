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

-- TVC05 : OCTO932 : Test correct results when VALUES clause is used in an expression (in WHERE clause etc.)

-- Below queries are copied from various comments in https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/932

select * from names n1 where id in (values (id*2), (id*2-1));
select * from names where id in (values (id*2-1), (3));
select * from names where id = (values(id*2-1));

