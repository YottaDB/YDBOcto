-- ######################################################################
-- #									#
-- # Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.		#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

-- TPGC011 : The PostgreSQL copy of the table in TPGC011.sql, to run the TPGC011 Go programs against PostgreSQL too

CREATE TABLE TPGC011 (id INTEGER PRIMARY KEY, v VARCHAR);
INSERT INTO TPGC011 VALUES (1, 'PAT'), (2, 'PATX'), (3, 'ZX'), (4, 'AX');
