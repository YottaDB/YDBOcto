#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSC17 : #337 : Test of MODULO (%) operator

select 3%2, 4%2 from names limit 1;
select id%3 from names;
select 1+3%2 from names limit 1;
select (1+3)%2 from names limit 1;
select 2*3%2 from names limit 1;
select (1+id)*2%5 from names;
