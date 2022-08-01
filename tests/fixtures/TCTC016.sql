#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCTC016 : OCTO519 : Test 63-character constraint names plus double-quotes are accepted without truncation

drop table if exists products;
CREATE TABLE products (product_no integer CONSTRAINT "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz12345678901" PRIMARY KEY, name text CONSTRAINT c2 UNIQUE CONSTRAINT c3 NOT NULL);
\d products;
select * from products;
