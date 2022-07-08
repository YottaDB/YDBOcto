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

-- TT02 : OCTO609 : TRUNCATE does not delete data from READONLY tables

-- All rows present in `names`, `pg_type`, and `customers` tables
select firstname, lastname from names;
select typname from pg_type;
select first_name, last_name from customers;

-- Single table case
-- All rows present in `pg_type` table
truncate pg_type;  -- ERR_TABLE_READONLY
-- All rows present in `pg_type` table
select typname from pg_type;

-- Multiple table case
truncate names, pg_type, customers;  -- ERR_TABLE_READONLY for `pg_type`
-- All rows present in `pg_type`, but deleted from `names` and `customers` tables
select firstname, lastname from names;
select typname from pg_type;
select first_name, last_name from customers;
