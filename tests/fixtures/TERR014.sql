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

-- TERR014 : Error underline is correct when there are extra newlines between queries (only possible with octo -f)

select * from aaaa;

select * from aaaa;




select * from aaaa;





select * from

aaaa;
