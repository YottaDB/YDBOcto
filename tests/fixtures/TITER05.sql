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
-- TITER05 : ITERATOR Mix ITERATOR key fields with regular key fields
CREATE TABLE cat_rec_titles
(
        catsys VARCHAR(30),
        id integer,
        idx integer ITERATOR "$$idx^TITER01",
        language VARCHAR(10) EXTRACT "RAti(keys(""idx""),""lg"")",
        type varchar(10) EXTRACT "RAti(keys(""idx""),""ty"")",
        source varchar(10) EXTRACT "RAti(keys(""idx""),""so"")",
        title varchar(200) EXTRACT "RAti(keys(""idx""),""ti"")",
        PRIMARY KEY (catsys,id,idx)
)
DELIM "^"
GLOBAL "^BCAT(keys(""catsys""),keys(""id""),""title"",keys(""idx""))";
select * from cat_rec_titles;
