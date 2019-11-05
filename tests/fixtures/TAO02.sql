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

-- Test type mismatch between operands of ALL/ANY/SOME. Should issue error.
select * from names n1 where n1.firstname =  ALL (select n2.id from names n2 where n1.id > n2.id + 3);
select * from names n1 where n1.id =  ALL (select * from names n2 where n1.id > n2.id + 3);

-- Test more than 1 column in sub-query of ALL/ANY/SOME. Should issue error.
select * from names n1 where n1.firstname =  ALL (select * from names n2 where n1.id > n2.id + 3);

