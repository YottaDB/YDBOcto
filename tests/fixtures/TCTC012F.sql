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
-- F: Table UNIQUE unnamed constraint on multiple NOT NULL fields (like B, except unnamed)
DROP TABLE IF EXISTS balances;
CREATE TABLE balances(
	distribution_id INT NOT NULL,
	address CHAR(32) NOT NULL,
	usd_balance DECIMAL(15, 4) NOT NULL,
	details TEXT,
	UNIQUE (distribution_id, address)
);

INSERT INTO balances VALUES
(1,'aaa',22.3355,'foo'),
(2,'aaa',22.5566,NULL);
SELECT * from balances;
-- These two will fail
INSERT INTO balances VALUES (1,'aaa',22.3365,'boo');
UPDATE balances SET distribution_id = 1 where distribution_id = 2;
SELECT * from balances;
DROP TABLE balances;
