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

select NOT n1.lastname from names n1;
select NOT 1 from names n1;
select NOT 'test' from names n1;
select NOT n1.* from names n1;;
-- Test that a boolean typed operand (`id = 1`) is still not accepted by NOT if it is in table.* format
select NOT n1.* from (select id = 1 from names) n1;
