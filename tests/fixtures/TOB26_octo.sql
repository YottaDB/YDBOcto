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

-- TOB26 : OCTO1179 : the tables of TOB26.sql (Octo only: the READONLY table's global is set in M)
-- READONLY: its global ^TOB26ro holds k2 in several of the forms Octo reads as a boolean (see the zwrite above)
CREATE TABLE TOB26ro (k1 INTEGER, k2 BOOLEAN, v VARCHAR PIECE 1, PRIMARY KEY (k1, k2)) GLOBAL "^TOB26ro(keys(""k1""),keys(""k2""))" READONLY;
-- READWRITE: the same rows, which Octo stores with k2 as 0 or 1
CREATE TABLE TOB26rw (k1 INTEGER, k2 BOOLEAN, v VARCHAR, PRIMARY KEY (k1, k2));
INSERT INTO TOB26rw VALUES (1, true, 'a'), (1, false, 'b'), (2, false, 'c'), (2, true, 'd'), (3, true, 'e'), (3, false, 'g');
