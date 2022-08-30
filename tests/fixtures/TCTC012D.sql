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
-- D: UNIQUE Named constraint on a NOT NULL field
DROP TABLE IF EXISTS Customer;
CREATE TABLE Customer(
	Id INT PRIMARY KEY,
	Name VARCHAR(100) NOT NULL,
	EmailAddress VARCHAR(100) NOT NULL,
	CONSTRAINT Unq_Customer_Email UNIQUE(EmailAddress)
);
INSERT INTO Customer VALUES
(1, 'aaa', 'a@example.com'),
(2, 'bbb', 'b@example.com');
SELECT * FROM Customer;
-- Next two wil fail
INSERT INTO Customer VALUES (3, 'ccc', 'a@example.com');
UPDATE Customer SET EmailAddress = 'a@example.com' WHERE Id = 2;
-- Verify data is still the same
SELECT * FROM Customer;
DROP TABLE Customer;
