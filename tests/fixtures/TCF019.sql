#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF019 : OCTO345 : Include function name and return type in -vv DEBUG output

select current_schema();
select abs(-2*id) from names;
select replace('abcd' || 'efgh', 'efgh', 'abcd');
