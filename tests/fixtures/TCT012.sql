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

-- TCT012 : OCTO300 : Octo uses right most type in calculated columns rather than highest precision type

select 1.5::integer+2.5::numeric+"1abcd"::numeric from names;
select 1.5::integer+2.5::numeric+"1abcd"::integer from names;

