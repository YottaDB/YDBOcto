#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCTC001 : OCTO772 : Test CONSTRAINT name is accepted for PRIMARY KEY, UNIQUE and NOT NULL

drop table if exists products;
CREATE TABLE products (product_no integer CONSTRAINT c1 PRIMARY KEY, name text CONSTRAINT c2 UNIQUE CONSTRAINT c3 NOT NULL);
select * from products;

