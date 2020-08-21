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

-- This is invoked from Stage 1 of the TC030 or TDT04 subtest

-- TC030 : OCTO90 : Rerunning query after CREATE TABLE should recreate plans that relied on the recreated table
-- TDT04 : OCTO90 : DROP TABLE should delete db nodes for plans that relied on the dropped table

SELECT * FROM orders o LEFT JOIN customers c ON o.customer_id = c.customer_id;
SELECT * FROM orders o;
SELECT * FROM orders where order_date = '';
SELECT * FROM orders where order_amount = '';
SELECT * FROM customers c;
SELECT * FROM customers where first_name = '';

