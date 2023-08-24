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

-- TC080 : OCTO929 : Test that repeated auto upgrade runs work fine with user defined tables

-- Below queries verify that tables/function that were added to octo-seed.sql after YDBOcto#929 (23b2d60c) are still there
-- We take one representative function and table.
select pg_tablespace_location(1);
select * from pg_depend;

