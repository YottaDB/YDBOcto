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
-- TITER11 : ITERATOR key-fixing applies on INNER JOIN
DROP TABLE IF EXISTS names_iterator;
CREATE TABLE names_iterator (
        id INTEGER PRIMARY KEY ITERATOR "$$id^TITER11",
        firstName VARCHAR(30),
        lastName VARCHAR(30)
) GLOBAL "^names";
-- INNER JOIN on the ITERATOR key column. With key-fixing, "id" is set from "names" (the outer table)
-- and the ITERATOR function "$$id^TITER11" is not called to enumerate values for this column.
SELECT n.id, n.firstName, i.lastName FROM names n INNER JOIN names_iterator i ON n.id = i.id;
