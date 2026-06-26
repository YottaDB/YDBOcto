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

-- Re-asked after each change to ^ORD: the row universe as a full table scan sees it, then the
-- same table filtered through the cross reference of a column stored ABOVE the row and of one
-- stored below it. "customer" lives on ^ORD(order_id), one node shared by every line of an
-- order, so a single write to it has to move every row of that order at once.
-- The changes are made in M, as an application would (Octo never writes a READONLY table), so
-- from here on the triggers maintain the index, not the bulk build. A new line under an existing
-- order must also pick up a customer written before the line existed.
SELECT * FROM order_lines ORDER BY order_id, line_id;
SELECT * FROM order_lines WHERE customer IS NULL ORDER BY order_id, line_id;
SELECT * FROM order_lines WHERE ship_method IS NULL ORDER BY order_id, line_id;
