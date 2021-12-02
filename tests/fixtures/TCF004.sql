#################################################################
#								#
# Copyright (c) 2020-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF004 : OCTO345 : M intrinsic functions supported by CREATE FUNCTION

CREATE FUNCTION DOLLARZWRITE(VARCHAR) RETURNS VARCHAR AS $ZWRITE;
select DOLLARZWRITE(firstname) from names;
-- Intrinsic functions using abbreviated syntax supported
CREATE FUNCTION DOLLARLENGTH(VARCHAR) RETURNS VARCHAR AS $L;
select DOLLARLENGTH(firstname) from names;
