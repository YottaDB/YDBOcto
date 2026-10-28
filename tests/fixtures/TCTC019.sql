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

-- TCTC019 : CHECK constraint CASE with multiple WHEN branches survives an auto upgrade of its text definition

-- Below CASE expressions used to be stored as "THEN bWHEN" and "THEN nWHEN", which an auto upgrade failed to parse
CREATE TABLE TCTC019a (id INTEGER PRIMARY KEY, a VARCHAR, b BOOLEAN,
	CONSTRAINT ck CHECK (CASE WHEN a = 'Zero' THEN b WHEN a = 'One' THEN false ELSE true END));
CREATE TABLE TCTC019b (id INTEGER PRIMARY KEY, n INTEGER
	CONSTRAINT ck CHECK (CASE n WHEN 1 THEN n WHEN 2 THEN 0 ELSE 1 END > 0));
