#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCV056 : OCTO929 : Test that repeated auto upgrade runs work fine with user defined views

-- Remove user defined tables created by test "setup()" function
-- This is needed to ensure we see a ERR_CANNOT_CREATE_VIEW error (without the code fixes).
-- Or else we would see a ERR_DUPLICATE_PRIMARY_KEY_CONSTRAINT error due to these tables.
DROP TABLE names;
DROP TABLE nameswithages;

-- View definition cannot use any existing tables (or else ERR_DUPLICATE_PRIMARY_KEY_CONSTRAINT errorwill
-- show up as mentioned above. So we use VALUES clause below.
CREATE VIEW TCV056 as values(1);

