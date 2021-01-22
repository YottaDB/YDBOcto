#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF000 : OCTO671 : Test of ERR_CANNOT_CREATE_FUNCTION error.

CREATE FUNCTION ABS(INTEGER) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;
CREATE FUNCTION ABS(NUMERIC) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;

