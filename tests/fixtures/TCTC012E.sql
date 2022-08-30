#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TCTC012 : OCTO582 : Various User Level UNIQUE Tests
-- E: Table UNIQUE unnamed constraint on a NOT NULL field
DROP TABLE IF EXISTS User2;
CREATE TABLE User2
(
	    id               INT          NOT NULL PRIMARY KEY,
	    firstnam         VARCHAR(255) NOT NULL,
	    lastname         VARCHAR(255) NOT NULL,
	    email            VARCHAR(255) NOT NULL,
	    UNIQUE (email)
);
INSERT INTO User2 VALUES (1,'a','b','a@example.com'), (2,'a','b','b@example.com');
SELECT * FROM User2;
-- These two will fail
INSERT INTO User2 VALUES (3,'a','b','a@example.com');
UPDATE User2 SET email = 'a@example.com' WHERE id = 2;
SELECT * FROM User2;
\d User2;
DROP TABLE User2;
