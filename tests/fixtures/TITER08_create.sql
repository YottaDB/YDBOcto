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
-- Silent setup for TITER08. Loaded with "load_fixture ... " (no "subtest") so the
-- octo-seed.sql parse does not flood the verbose output.txt that captures the
-- subsequent TITER08.sql queries.
DROP TABLE IF EXISTS cat_rec_titles;
CREATE TABLE cat_rec_titles
(
        catsys VARCHAR(30) ITERATOR "$$catsys^TITER08",                        -- backed by ^catrectitles
        id integer ITERATOR "$$id^TITER08" VIRTUAL,                            -- synthetic
        idx integer ITERATOR "$$idx^TITER08" VIRTUAL,                          -- synthetic; populates RAti as a side effect
        language VARCHAR(10) EXTRACT "RAti(""lg"")",
        type VARCHAR(10) EXTRACT "RAti(""ty"")",
        source VARCHAR(10) EXTRACT "RAti(""so"")",
        title VARCHAR(200) EXTRACT "RAti(""ti"")",
        PRIMARY KEY (catsys,id,idx)
) GLOBAL "^catrectitles";
