#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Every question TX22.sql asks, re-asked. The first is the row universe as a full table scan
-- sees it; the other three are that same table filtered through a cross reference, one per
-- column, at each of the depths this table stores columns at. They must agree after every
-- change to ^USER: a row in the scan is a row in all three indexes, and a NULL is a NULL.
-- The changes are made in M, as an application would (Octo never writes a READONLY table), so
-- from here on the triggers maintain the index, not the bulk build. This is the table worth doing
-- that to: its columns sit at two different depths below the row, so the record anchor spans two
-- levels and AIM plants a record trigger pair at each. A row created or destroyed by a write at
-- either depth is noticed only if the anchor spans that far, and an anchor one level too short
-- still builds a perfectly correct index and then silently stops maintaining it, which no
-- build-and-assert test can see.
SELECT * FROM users ORDER BY id;
SELECT * FROM users WHERE email IS NULL ORDER BY id;
SELECT * FROM users WHERE dept IS NULL ORDER BY id;
SELECT * FROM users WHERE name IS NULL ORDER BY id;
