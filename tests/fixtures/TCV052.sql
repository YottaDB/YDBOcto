#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

create function absf(integer) returns integer AS $$ABS^%ydboctosqlfunctions;
create view v1 as values (1), (ABSF(-2));
drop function ABSF(integer);
create view v2 as values (ABSF(-2)), (1);
drop function ABSF(integer);
