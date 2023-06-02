#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

DROP TABLE IF EXISTS customersTF03;
CREATE TABLE customersTF03 (customer_id INTEGER PRIMARY KEY,first_name VARCHAR(8),last_name VARCHAR(10),email VARCHAR(20),address VARCHAR(26),city VARCHAR(16),state VARCHAR(2),zipcode VARCHAR(5));
INSERT INTO customersTF03 (SELECT * FROM customers);
-- The following query results in a NOT NULL constraint failure and it is expected to be processed by the sed in test_helpers.bats.in responsible to convert them to a comparable format works fine
-- It failed in pipeline couple of times at one time
INSERT INTO customersTF03  (last_name,email) (VALUES('Madison','jadams@usa.gov'));
