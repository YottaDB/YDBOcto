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
INSERT INTO names VALUES (12, 'First12', 'Last12'), (13, 'First13', 'Last13');
-- Also test INSERT INTO with comma-separated list of columns specified for the target table
INSERT INTO names(id,firstname,lastname) VALUES (14, 'First14', 'Last14'), (15, 'First15', 'Last15') UNION SELECT id+16,'First' || (id+16), 'Last' || (id+16) FROM names LIMIT 1;
INSERT INTO names VALUES (17, 'First17', 'Last17'), (18, 'First18', 'Last18') UNION VALUES (19, 'First19', 'Last19'), (20, 'First20', 'Last20');
SELECT ' -- NAMES table AFTER various INSERT INTO operations; Has 21 rows, id=0 thru id=20';
SELECT * FROM names;

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

