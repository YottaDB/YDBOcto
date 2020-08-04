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

-- TDF001 : OCTO345 : DROP FUNCTION works with new function

CREATE FUNCTION DOLLARZWRITE(VARCHAR) RETURNS VARCHAR AS $ZWRITE;
select DOLLARZWRITE(firstname) from names;
DROP FUNCTION DOLLARZWRITE (VARCHAR);
select DOLLARZWRITE(firstname) from names;

