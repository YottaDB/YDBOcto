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

-- TBT01: Test of BOOLEAN type across various valid queries in `boolean` database

-- Test that boolean values obtained from sub-queries compare fine against boolean literals in outer queries
SELECT * FROM stock_availability a1 WHERE a1.available = (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id);
SELECT * FROM stock_availability a1 WHERE a1.available != (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id);
SELECT * FROM stock_availability a1 WHERE a1.available = (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1);
SELECT * FROM stock_availability a1 WHERE a1.available != (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1);
SELECT a1.available = (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id) FROM stock_availability a1;
SELECT a1.available != (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id) FROM stock_availability a1;
SELECT a1.available = (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1) FROM stock_availability a1;
SELECT a1.available != (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1) FROM stock_availability a1;

