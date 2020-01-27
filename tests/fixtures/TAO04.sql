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

-- TAO04 : OCTO448 : Incorrect results from ALL/ANY when empty string is in the list

select * from names where lastname <= ALL (select ''::varchar);
select * from names where lastname > ANY (select ''::varchar);

