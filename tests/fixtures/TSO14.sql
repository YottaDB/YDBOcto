#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSO14 : OCTO449 : Incorrect results from UNION/INTERSECT/EXCEPT when inside IN operator and empty string is in the list

select * from names n1 where lastname IN ((select ''::varchar from names limit 1) UNION (select 'Cool' from names limit 1));
select * from names n1 where lastname IN ((select ''::varchar from names limit 1) UNION ALL (select 'Cool' from names limit 1));
select * from names n1 where lastname IN ((select ''::varchar from names limit 1) INTERSECT (select n3.lastname from names n3));
select * from names n1 where lastname IN ((select ''::varchar from names limit 1) INTERSECT ALL (select n3.lastname from names n3));
select * from names n1 where lastname IN ((select n2.lastname from names n2) EXCEPT (select 'Cool' from names n3));
select * from names n1 where lastname IN ((select n2.lastname from names n2) EXCEPT ALL (select 'Cool' from names n3));

