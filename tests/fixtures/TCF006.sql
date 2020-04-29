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

-- TCF006 : OCTO345 : M intrinsic functions created by CREATE FUNCTION are case sensitive" {

-- Map function to intrinsic function (all caps)
CREATE FUNCTION DOLLARZWRITE(VARCHAR) RETURNS VARCHAR AS $ZWRITE;
select DOLLARZWRITE(firstname) from names;

-- Map function to intrinsic function (lowercase)
CREATE FUNCTION DOLLARZWRITE(VARCHAR) RETURNS VARCHAR AS $zwrite;
select DOLLARZWRITE(firstname) from names;

-- Map function to intrinsic function (mixed case)
CREATE FUNCTION DOLLARZWRITE(VARCHAR) RETURNS VARCHAR AS $zWrItE;
select DOLLARZWRITE(firstname) from names;

