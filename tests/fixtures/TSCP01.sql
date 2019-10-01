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

-- TSCP01 : OCTO466 : Incorrect results when `|` is part of the column value in the SELECT column list

select id,'|',firstname from names where id < 2;
select * from (select id,'|',firstname from names where id < 2) n1;

