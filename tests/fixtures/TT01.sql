#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TT01 : OCTO609 : TRUNCATE deletes all row data for given tables

-- Single table case
-- All rows present in `names` table
select * from names;
truncate names;
-- No rows present in `names` table, but table still exists
select * from names;

-- Multiple table case
-- All rows present in `customers` and `orders` tables
select * from customers;
select * from orders;
truncate customers, orders;
-- No rows present in `customers` or `orders` tables, but tables still exist
select * from customers;
select * from orders;

-- Non-existent tables case
truncate badtable, notgoodtable, missingtable;
