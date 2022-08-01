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

-- TSO06 : EXCEPT ALL removes the same count of items from the first set as exist in the second set

select firstName, lastName from names
except all
select firstName, lastName from names where firstName = 'Zero' and id = 0;

