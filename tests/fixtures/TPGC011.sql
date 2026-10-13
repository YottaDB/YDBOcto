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

-- TPGC011 : OCTO1165 : Bind parameters $n are numbered as in PostgreSQL: $n gets the n-th value of the Bind message,
-- whatever the order of the bind parameters in the query, and the same $n may be used more than once. Before
-- YDBOcto#1165, rocto numbered them in the order in which they occur in the query, so a query with $2 before $1 got
-- the values swapped, and a query that used $1 twice needed two values.

CREATE TABLE TPGC011 (id INTEGER PRIMARY KEY, v VARCHAR) GLOBAL "^TPGC011";
INSERT INTO TPGC011 VALUES (1, 'PAT'), (2, 'PATX'), (3, 'ZX'), (4, 'AX');
SELECT * FROM TPGC011 ORDER BY id;
