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

-- Re-asked after each change to ^EVT. "active" is a BOOLEAN, so AIM indexes it through a
-- transformation function - type 6 rather than 4. The transform has to be applied by the
-- triggers on every write, not only by the initial scan, and it has to be applied to the
-- absent node as well: a row with no ^EVT(id,"ACT") is SQL NULL, not FALSE.
-- The changes are made in M, as an application would (Octo never writes a READONLY table), so
-- from here on the triggers maintain the index, not the bulk build.
SELECT * FROM events ORDER BY id;
SELECT * FROM events WHERE active IS NULL ORDER BY id;
SELECT * FROM events WHERE active = TRUE ORDER BY id;
SELECT * FROM events WHERE active = FALSE ORDER BY id;
