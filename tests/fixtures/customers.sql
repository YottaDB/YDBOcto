#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE TABLE customers (customer_id INTEGER PRIMARY KEY, first_name VARCHAR(30), last_name VARCHAR(30), email VARCHAR(64), address VARCHAR(128), city VARCHAR(32), state VARCHAR(2), zipcode VARCHAR(10)) GLOBAL "^customers(keys(""customer_id""))";
CREATE TABLE orders (order_id INTEGER PRIMARY KEY, order_date VARCHAR(30), order_amount VARCHAR(30), customer_id INTEGER) GLOBAL "^orders(keys(""order_id""))";
