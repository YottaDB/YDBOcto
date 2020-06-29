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
-- TCEF01 : OCTO539 : COALESCE

-- The queries in this query file are either invalid queries
-- or do not have the same behavior as postgres.

-- Passing no arguments is an error
SELECT COALESCE();
-- Invalid column name
SELECT COALESCE(invalid_column) FROM names;
-- Unlike in postgres, different types are allowed.
SELECT COALESCE(NULL, 'a', 1.2);
SELECT COALESCE('a', 1.2, 1);
SELECT COALESCE(NULL, 1, 'a');
