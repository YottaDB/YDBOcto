#################################################################
#								#
# Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDF005 : OCTO562 : DROP FUNCTION on overloaded function only deletes targeted definition

CREATE FUNCTION ABSTEST(INTEGER) RETURNS INTEGER AS $$samevalue^functions;
CREATE FUNCTION ABSTEST(NUMERIC) RETURNS NUMERIC AS $$samevalue^functions;

-- Attempt to DROP a definition that doesn't exist for a function that DOES exist
DROP FUNCTION ABSTEST(VARCHAR);

-- DROP only one definition, i.e. ABSTEST (INTEGER), of an overloaded function.
-- One definition, i.e. ABSTEST(NUMERIC),  should remain afterward, per CREATE FUNCTION calls in octo-seed.sql.
DROP FUNCTION ABSTEST(INTEGER);

