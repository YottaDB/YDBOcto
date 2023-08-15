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
-- TITER09 : ITERATOR shown in \d output
DROP TABLE IF EXISTS names;
CREATE TABLE names (id INTEGER PRIMARY KEY ITERATOR "$$id^names", firstName VARCHAR(30), lastName VARCHAR(30)) GLOBAL "^names";
\d names;
-- Composite primary key with multiple ITERATOR columns
DROP TABLE IF EXISTS composite;
CREATE TABLE composite (a INTEGER ITERATOR "$$a^TITER09", b INTEGER ITERATOR "$$b^TITER09", val VARCHAR(30), PRIMARY KEY (a, b)) GLOBAL "^composite(keys(""a""),keys(""b""))";
\d composite;
