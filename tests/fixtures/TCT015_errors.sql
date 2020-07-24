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
-- TCT015 : OCTO475 : Multiple usages of the :: (typecast) operator should be accepted
-- This change also allowed multiple array indexes
-- However, Octo doesn't yet implement arrays, so this only checks that it parses, not that it does anything.
select 1[1];
select 1[1][1];
select (1[1])[1];
