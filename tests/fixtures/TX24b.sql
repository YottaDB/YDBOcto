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

-- Re-asked after each change to ^G or ^H. The rows live in ^G and both indexed columns live in
-- ^H, so the two sides of the anchor are in different globals: a write to ^H must move a row
-- that ^G defines, and a write to ^G must pick up a value that only ^H holds. Writing ^H where
-- ^G has no row must not invent one.
-- The changes are made in M, as an application would (Octo never writes a READONLY table), so
-- from here on the triggers maintain the index, not the bulk build. Each direction is checked
-- separately below, since the two sides of the anchor are in different globals.
SELECT * FROM order_lines ORDER BY order_id, line_id;
SELECT * FROM order_lines WHERE cust IS NULL ORDER BY order_id, line_id;
SELECT * FROM order_lines WHERE note IS NULL ORDER BY order_id, line_id;
