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

-- TCF007 : OCTO345 : SQL functions created by CREATE FUNCTION are case insensitive" {
CREATE FUNCTION DOLLARZWRITE(VARCHAR) RETURNS VARCHAR AS $ZWRITE;

-- Call function with name in all caps
select DOLLARZWRITE(firstname) from names;

-- Call function with name in lowercase
select dollarzwrite(firstname) from names;

-- Call function with name in mixed case
select dollarZWRITE(firstname) from names;

