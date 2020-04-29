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

-- TCF001 : OCTO345 : Allow specification of type for parameters and return value of user-defined functions

CREATE FUNCTION ABS(NUMERIC) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;
select ABS(-id)+2 as absid from names;
