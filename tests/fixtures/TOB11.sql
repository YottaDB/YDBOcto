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

-- TOB11 : OCTO390 : ORDER BY does not work correctly for empty string values when more than one column is specified

select * from (select * from names union select 6::integer,'Joey','Abcd' union select 7::integer,'Joey','Zzzz') subquery1 order by firstname desc, lastname asc;

