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
-- TITER03 : ITERATOR Disallowed on non-key fields
CREATE TABLE cat_rec_titles
(
        catsys VARCHAR(30),
        id integer,
        idx integer,
        language VARCHAR(10) ITERATOR "$$idx^bcat" EXTRACT "RAti(keys(""idx""),""lg"")",
        PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),""title"",keys(""idx""))";
