#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TIIT04 : OCTO502 : Simple INSERT INTO queries work

SELECT '';
SELECT ' -- NAMES table BEFORE INSERT INTO : Has 6 rows';
SELECT * FROM names;
SELECT '';
SELECT ' -- INSERT INTO';
SELECT ' -- Add every existing row of NAMES table into same table resulting in 2x rows';
INSERT INTO names SELECT id+6,firstname,lastname FROM names;
SELECT '';
SELECT ' -- NAMES table AFTER INSERT INTO : Has 12 rows';
SELECT * FROM names;
INSERT INTO names values (12, 'First', 'Last'), (13, 'First2', 'Last2');
-- Below query can be enabled once YDBOcto#623 is fixed (currently assert fails)
-- INSERT INTO names values (14, 'First3', 'Last3'), (15, 'First4', 'Last4') UNION SELECT id+16,'First' || (id+16), 'Last' || (id+16) FROM names;
INSERT INTO names values (14, 'First14', 'Last14'), (15, 'First15', 'Last15') UNION values (16, 'First16', 'Last16'), (17, 'First17', 'Last17');

SELECT '';
SELECT ' -- CUSTOMERS table BEFORE INSERT INTO : Has 5 rows';
SELECT * FROM customers;
SELECT '';
SELECT ' -- INSERT INTO';
SELECT ' -- Add every existing row of CUSTOMERS table into same table resulting in 2x rows';
INSERT INTO customers SELECT customer_id+5,first_name,last_name,email,address,city,state,zipcode FROM customers;
SELECT '';
SELECT ' -- CUSTOMERS table AFTER INSERT INTO : Has 10 rows (twice original number)';
SELECT * FROM customers;

SELECT '';
SELECT ' -- COMPOSITE table BEFORE INSERT INTO : Has 10 rows';
SELECT * FROM composite;
SELECT '';
SELECT ' -- INSERT INTO';
SELECT ' -- Add every existing row of COMPOSITE table into same table resulting in 2x rows';
INSERT INTO composite SELECT id0,id1,id2,id3,id4,id5+2,id6,id7,name FROM composite;
SELECT '';
SELECT ' -- COMPOSITE table AFTER INSERT INTO : Has 20 rows (twice original number)';
SELECT * FROM composite;

