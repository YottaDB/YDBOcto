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

select distinct n2.* from names n1, names n2 order by n2.*;

select (select id from names n2 ORDER BY n1.id LIMIT 1),count(n1.id) from names n1;
