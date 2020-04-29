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

-- TCF013 : OCTO345 : Sub-queries in expressions work with CREATEd functions

select ABS(-id)+(select 2 from names limit 1) as absid from names;
select ABS(-id)-(select 2 from names limit 1) as absid from names;
select ABS(-id)*(select 2 from names limit 1) as absid from names;
select ABS(-id)/(select 2 from names limit 1) as absid from names;
select ABS(-id)%(select 2 from names limit 1) as absid from names;
