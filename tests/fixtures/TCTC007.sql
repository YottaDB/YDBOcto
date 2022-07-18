#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCTC007 : OCTO772 : Test col(i) usages are optimized in generated M code for CHECK constraint (in UPDATE command)

CREATE TABLE tmp (id1 integer PRIMARY KEY CHECK (id4 != 0) CHECK ((id2 != 1) and (id4 = 2)) CHECK ((id1 != 0) and (id5 != 0)), id2 integer, id3 integer, id4 integer, id5 integer);
UPDATE tmp SET id1 = 10, id4 = 20;

