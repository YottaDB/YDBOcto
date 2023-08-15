#################################################################
#								#
# Copyright (c) 2023-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TITER04 : ITERATOR Disables Cross References/ITERATOR JOINS
DROP TABLE IF EXISTS names_iterator;
CREATE TABLE names_iterator (
        id INTEGER PRIMARY KEY ITERATOR "$$id^TITER04",
        firstName VARCHAR(30),
        lastName VARCHAR(30)
) GLOBAL "^names";
