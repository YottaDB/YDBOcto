#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TBT06: Test of BOOLEAN IS comparisons various valid queries in `boolean` database

-- Test random queries that failed other tests with #498 changes (i.e. TTA002)
SELECT 1 FROM (VALUES (2, NULL)) n1 WHERE NOT n1.* IS NOT NULL;
-- Test comparison of BOOLEAN field against each IS and IS NOT scenario, i.e. TRUE, FALSE, UNKNOWN
SELECT * FROM stock_availability WHERE available IS TRUE;
SELECT * FROM stock_availability WHERE available IS NOT TRUE;
SELECT * FROM stock_availability WHERE available IS FALSE;
SELECT * FROM stock_availability WHERE available IS NOT FALSE;
SELECT * FROM stock_availability WHERE available IS UNKNOWN;
SELECT * FROM stock_availability WHERE available IS NOT UNKNOWN;
SELECT * FROM stock_availability a1 WHERE a1.available IS TRUE;
SELECT * FROM stock_availability a1 WHERE a1.available IS NOT TRUE;
SELECT * FROM stock_availability a1 WHERE a1.available IS FALSE;
SELECT * FROM stock_availability a1 WHERE a1.available IS NOT FALSE;
SELECT * FROM stock_availability a1 WHERE a1.available IS UNKNOWN;
SELECT * FROM stock_availability a1 WHERE a1.available IS NOT UNKNOWN;
-- Test comparison of BOOLEAN fields in subqueries against each IS and IS NOT scenario, i.e. TRUE, FALSE, UNKNOWN
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id) IS TRUE FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id) IS NOT TRUE FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1) IS TRUE FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1) IS NOT TRUE FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id) IS FALSE FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id) IS NOT FALSE FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1) IS FALSE FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1) IS NOT FALSE FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id) IS UNKNOWN FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id = a2.product_id) IS NOT UNKNOWN FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1) IS UNKNOWN FROM stock_availability a1;
SELECT (SELECT a2.available from stock_availability a2 where a1.product_id != a2.product_id ORDER BY a2.product_id LIMIT 1) IS NOT UNKNOWN FROM stock_availability a1;

-- Test IS UNKNOWN followed by literals works fine when reusing similar IS NULL query plan
-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/498#note_1623006175
select true is null;
select true is unknown;
select true is null,3;
select true is unknown,3;

