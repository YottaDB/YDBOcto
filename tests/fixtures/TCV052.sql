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
create view v1 as values (1), (ABS(-2));
drop function ABS(integer);
create view v2 as values (ABS(-2)), (1);
drop function ABS(integer);
