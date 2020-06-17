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

-- TAK06 : OCTO497 : Support column aliases in SELECT column list without AS keyword

select 1 as n1;
select 1 n1;
select 1 as "n1";
select 1 "n1";
