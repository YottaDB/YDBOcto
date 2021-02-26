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


-- TAS00 : ARRAY constructor subquery syntax

-- ARRAY syntax in SELECT list
SELECT ARRAY(SELECT NULL FROM names);
SELECT ARRAY(SELECT firstname FROM names);
SELECT ARRAY(SELECT id FROM names) from names;
SELECT ARRAY(SELECT firstname FROM names ORDER BY id) from names;
SELECT ARRAY(SELECT firstname FROM names ORDER BY id), ARRAY(SELECT lastname FROM names) from names;
SELECT CONCAT('My array: ', ARRAY(SELECT firstname FROM names ORDER BY id)::varchar) from names;
SELECT CONCAT('My array: ', ARRAY(SELECT lastname FROM names ORDER BY id)::varchar) from names;

