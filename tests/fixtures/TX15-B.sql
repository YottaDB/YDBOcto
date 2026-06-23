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
-- MIN/MAX reads straight off the AIM index (YDBOcto#617); "#"-prefixed string subscripts collate
-- lexically. ^ppfruit is "x|<fruit>^<extra>|z"; expect lo=Apple, hi=Mango.
CREATE TABLE ppfruit (id INTEGER PRIMARY KEY, fruit VARCHAR DELIMS ("|","^") PIECES (2,1)) GLOBAL "^ppfruit(keys(""id""))";
SELECT * FROM ppfruit ORDER BY id;
SELECT MIN(fruit) AS lo, MAX(fruit) AS hi FROM ppfruit;
-- Same, 3 levels deep: ^ppfruit3 is "a|b^c~<fruit>~e^f|g".
CREATE TABLE ppfruit3 (id INTEGER PRIMARY KEY, fruit VARCHAR DELIMS ("|","^","~") PIECES (2,2,2)) GLOBAL "^ppfruit3(keys(""id""))";
SELECT * FROM ppfruit3 ORDER BY id;
SELECT MIN(fruit) AS lo, MAX(fruit) AS hi FROM ppfruit3;
