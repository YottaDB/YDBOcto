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

-- Re-asked after each change, whichever database the change was made in. The rows are in the
-- local ^G and "cust" is in ^H in extended.gld, so the record-side triggers and the value-side
-- triggers live in different DATABASES. Each has to fire on a write to its own database and
-- reach across to read the other.
-- The changes are made in M, as an application would (Octo never writes a READONLY table), so
-- from here on the triggers maintain the index, not the bulk build. Note which gld each step
-- runs under: that is the point of the step.
SELECT * FROM xorder_lines ORDER BY order_id, line_id;
SELECT * FROM xorder_lines WHERE cust IS NULL ORDER BY order_id, line_id;
SELECT * FROM xorder_lines WHERE cust = 'Alice' ORDER BY order_id, line_id;
