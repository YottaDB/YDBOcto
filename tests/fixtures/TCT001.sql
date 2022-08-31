#################################################################
#								#
# Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCT001: coerce canonical number string to INTEGER

select '1'::integer from names limit 1;
select '1.1'::integer from names limit 1;
select * from names where '1'::integer = names.id;
select * from names where '1.1'::integer = names.id;
