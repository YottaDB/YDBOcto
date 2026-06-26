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

-- Both questions re-asked: the row universe as a full table scan sees it, then the same
-- table filtered through the cross reference. They must agree after every change to ^ORD.
-- The changes are made in M, as an application would (Octo never writes a READONLY table), so
-- from here on the SET and ZKILL triggers maintain the index, not the bulk build.
SELECT * FROM order_lines ORDER BY order_id, line_id;
SELECT * FROM order_lines WHERE item IS NULL ORDER BY order_id, line_id;
