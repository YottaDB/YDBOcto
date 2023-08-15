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
-- TITER07 : Garbage In Garbage Out
DROP TABLE IF EXISTS cat_rec_titles;
CREATE TABLE cat_rec_titles
(
        catsys VARCHAR(30) ITERATOR "quit:foo",
        id integer ITERATOR "n x",
        idx integer ITERATOR "xxx aswsdf",
        language VARCHAR(10) EXTRACT "$get(RAti(keys(""idx""),""lg""))",
        type VARCHAR(10) EXTRACT "$get(RAti(keys(""idx""),""ty""))",
        source VARCHAR(10) EXTRACT "$get(RAti(keys(""idx""),""so""))",
        title VARCHAR(200) EXTRACT "$get(RAti(keys(""idx""),""ti""))",
        PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),""title"",keys(""idx""))";
select * from cat_rec_titles;
