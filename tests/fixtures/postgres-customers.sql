-- ######################################################################
-- #									#
-- # Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

-- Below is to skip the INSERT commands if the table already exists (CREATE TABLE will cause an error and script will exit)
\set ON_ERROR_STOP on

CREATE TABLE customers (customer_id INTEGER PRIMARY KEY, first_name CHAR(8), last_name CHAR(10), email CHAR(20), address CHAR(26), city CHAR(16), state CHAR(2), zipcode CHAR(5));
CREATE TABLE orders (order_id INTEGER PRIMARY KEY, order_date CHAR(10), order_amount VARCHAR(7), customer_id INTEGER);

INSERT INTO customers VALUES (1,'George','Washington','gwashington@usa.gov','3200 Mt Vernon Hwy','Mount Vernon','VA','22121');
INSERT INTO customers VALUES (2,'John','Adams','jadams@usa.gov','1250 Hancock St','Quincy','MA','02169');
INSERT INTO customers VALUES (3,'Thomas','Jefferson','tjefferson@usa.gov','931 Thomas Jefferson Pkwy','Charlottesville','VA','22902');
INSERT INTO customers VALUES (4,'James','Madison','jmadison@usa.gov','11350 Constitution Hwy','Orange','VA','22960');
INSERT INTO customers VALUES (5,'James','Monroe','jmonroe@usa.gov','2050 James Monroe Parkway','Charlottesville','VA','22902');

INSERT INTO orders VALUES (1,'07/04/1776','$234.56',1);
INSERT INTO orders VALUES (2,'03/14/1760','$78.50',3);
INSERT INTO orders VALUES (3,'05/23/1784','$124.00',2);
INSERT INTO orders VALUES (4,'09/03/1790','$65.50',3);
INSERT INTO orders VALUES (5,'07/21/1795','$25.50',10);
INSERT INTO orders VALUES (6,'11/27/1787','$14.40',9);
