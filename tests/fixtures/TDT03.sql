#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/OR its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDT03 : OCTO439 : DROP TABLE (and CREATE TABLE) also deletes unnecessary global variables nodes from catalog

-- Below query prints the names of ALL columns of the table called 'CUSTOMERS' using the catalog.

SELECT c.relname, a.attname
FROM pg_catalog.pg_class AS c
INNER JOIN pg_catalog.pg_attribute AS a ON a.attrelid = c.oid
WHERE c.relname = 'ORDERS';

