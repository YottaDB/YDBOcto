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
-- TITER02 : ITERATOR Disallowed keywords (START/STARTINCLUDE/END/ENDPOINT/AIMTYPE)
DROP TABLE IF EXISTS cat_rec_titles;
CREATE TABLE cat_rec_titles
(
        catsys VARCHAR(30) ITERATOR "$$catsys^TITER01" START 0,
        PRIMARY KEY (catsys)
);
CREATE TABLE cat_rec_titles
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01" STARTINCLUDE,
        PRIMARY KEY (catsys)
);
CREATE TABLE cat_rec_titles
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01" ENDPOINT 0,
        PRIMARY KEY (catsys)
);
CREATE TABLE cat_rec_titles
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01" END "''$$catsys^TITER01(5)",
        PRIMARY KEY (catsys)
);
CREATE TABLE cat_rec_titles
(
	catsys VARCHAR(30) ITERATOR "$$catsys^TITER01",
        PRIMARY KEY (catsys)
)
AIMTYPE 1;
