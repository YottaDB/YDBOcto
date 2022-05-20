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

select NULL in (n1.*) from names n1 group by NULL in (n1.*) having n1.* in (NULL,NULL);
select NULL in (n1.*) from names n1 group by 1 having n1.* in (NULL,NULL);
