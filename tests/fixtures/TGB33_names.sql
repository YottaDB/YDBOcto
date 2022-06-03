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

-- #819 related to #807
select 1 from names n1 having exists (select 1 from names n2 where count(n1.firstname||'hello')=1);
select 1 from names n1 having exists (select 1 from names n2 where count(n1.firstname||n1.firstname)=1);
