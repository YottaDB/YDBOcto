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

-- TCT018 : OCTO636 : SIZE specified in the NUMERIC type of the typecast operator (::) should be honored

-- All below queries produce errors

-- Test of ERR_NUMERIC_SCALE
CREATE TABLE abcd(id NUMERIC(3,4));
SELECT 1.50::numeric(2,3);
SELECT -1.50::numeric(2,3);

-- Test of syntax error
CREATE TABLE abcd(id NUMERIC(3,-1));	-- negative scale is not allowed
SELECT 1.50::INTEGER(2,3);		-- scale parameter is not allowed for INTEGER
SELECT -1.50::INTEGER(2,3);		-- scale parameter is not allowed for INTEGER

-- Test of ERR_INVALID_INTEGER_SYNTAX
CREATE TABLE abcd(id NUMERIC(3,0.5));

-- Test of ERR_NUMERIC_OVERFLOW
SELECT 1495::NUMERIC(2);
SELECT -1495::NUMERIC(2);
SELECT 1.49::NUMERIC(0);
SELECT 15.54::NUMERIC(1);
SELECT 15.54::NUMERIC(0);
SELECT -1.49::NUMERIC(0);
SELECT -15.54::NUMERIC(1);
SELECT -15.54::NUMERIC(0);

