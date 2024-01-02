#################################################################
#								#
# Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE TABLE customers (customer_id INTEGER PRIMARY KEY, first_name VARCHAR(8), last_name VARCHAR(10), email VARCHAR(20), address VARCHAR(26), city VARCHAR(16), state VARCHAR(2), zipcode VARCHAR(5)) GLOBAL "^customers";
CREATE TABLE orders (order_id INTEGER PRIMARY KEY, order_date DATE, order_amount VARCHAR(7), customer_id INTEGER) GLOBAL "^orders" READONLY;
