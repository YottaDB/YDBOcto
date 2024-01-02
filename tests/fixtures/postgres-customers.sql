-- ######################################################################
-- #									#
-- # Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

CREATE TABLE customers (customer_id INTEGER PRIMARY KEY, first_name VARCHAR(8), last_name VARCHAR(10), email VARCHAR(20), address VARCHAR(26), city VARCHAR(16), state VARCHAR(2), zipcode VARCHAR(5));
CREATE TABLE orders (order_id INTEGER PRIMARY KEY, order_date DATE, order_amount VARCHAR(7), customer_id INTEGER);

INSERT INTO customers VALUES (1,'George','Washington','gwashington@usa.gov','3200 Mt Vernon Hwy','Mount Vernon','VA','22121');
INSERT INTO customers VALUES (2,'John','Adams','jadams@usa.gov','1250 Hancock St','Quincy','MA','02169');
INSERT INTO customers VALUES (3,'Thomas','Jefferson','tjefferson@usa.gov','931 Thomas Jefferson Pkwy','Charlottesville','VA','22902');
INSERT INTO customers VALUES (4,'James','Madison','jmadison@usa.gov','11350 Constitution Hwy','Orange','VA','22960');
INSERT INTO customers VALUES (5,'James','Monroe','jmonroe@usa.gov','2050 James Monroe Parkway','Charlottesville','VA','22902');

INSERT INTO orders VALUES (1,'1776/07/04','$234.56',1);
INSERT INTO orders VALUES (2,'1760/03/14','$78.50',3);
INSERT INTO orders VALUES (3,'1784/05/23','$124.00',2);
INSERT INTO orders VALUES (4,'1790/09/03','$65.50',3);
INSERT INTO orders VALUES (5,'1795/07/21','$25.50',10);
INSERT INTO orders VALUES (6,'1787/11/27','$14.40',9);
