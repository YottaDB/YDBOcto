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

-- TDF005 : OCTO562 : DROP FUNCTION on overloaded function only deletes targeted definition

-- Attempt to DROP a definition that doesn't exist for a function that DOES exist
DROP FUNCTION ABS (VARCHAR);

-- DROP only one definition, i.e. ABS (INTEGER), of an overloaded function.
-- One definition, i.e. ABS (NUMERIC),  should remain afterward, per CREATE FUNCTION calls in octo-seed.sql.
DROP FUNCTION ABS (INTEGER);

