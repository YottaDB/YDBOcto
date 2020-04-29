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

-- TCF002 : OCTO345 : Report syntax errors for function call parameter type mismatches
select ABS('balloons') as badarg from names;

select REPLACE('balloons', 12, 'hot air') as badarg from names;

select PG_CATALOG.PG_GET_EXPR('balloons', true) as badarg from names;

