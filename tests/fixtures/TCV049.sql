#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

create view TCV049_v12 as select * from names UNION select * from names;
create view TCV049_v13 as select * from names EXCEPT ALL select * from TCV049_v12;
create view TCV049_v11 as select * from names EXCEPT select * from TCV049_v12;
