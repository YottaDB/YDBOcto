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

-- ORDER BY having reference to parent table `alias1` in exists clause
SELECT alias1.*, alias1.last_name FROM customers CROSS JOIN (SELECT DISTINCT alias1.last_name FROM customers alias1 ORDER BY alias1.last_name) AS alias1 INNER JOIN (SELECT alias3.last_name, alias3.email FROM customers alias3 ORDER BY alias3.email, alias3.last_name) AS alias3 ON (((alias1.last_name <= alias3.last_name) AND NOT (alias1.last_name > ALL (SELECT ALL alias4.first_name FROM customers alias4 ORDER BY alias4.first_name LIMIT 1)))) WHERE NOT EXISTS (SELECT MAX(ALL alias6.email) as email, alias6.last_name FROM customers alias6 GROUP BY alias6.last_name, alias1.last_name, alias1.last_name HAVING MIN(alias6.email) LIKE '%uwl%#' ORDER BY MAX(ALL alias6.email), alias6.last_name, alias1.*) ORDER BY alias1.*, alias1.last_name;

-- Select column list having reference to parent table `alias1`
SELECT alias1.*, alias1.last_name FROM customers CROSS JOIN (SELECT DISTINCT alias1.last_name FROM customers alias1 ORDER BY alias1.last_name) AS alias1 INNER JOIN (SELECT alias3.last_name, alias3.email FROM customers alias3 ORDER BY alias3.email, alias3.last_name) AS alias3 ON (((alias1.last_name <= alias3.last_name) AND NOT (alias1.last_name > ALL (SELECT ALL alias4.first_name FROM customers alias4 ORDER BY alias4.first_name LIMIT 1)))) WHERE NOT EXISTS (SELECT MAX(ALL alias6.email) as email, alias6.last_name, alias1.* FROM customers alias6 GROUP BY alias6.last_name, alias1.last_name, alias1.last_name HAVING MIN(alias6.email) LIKE '%uwl%#' ORDER BY MAX(ALL alias6.email), alias6.last_name, alias1.last_name) ORDER BY alias1.*, alias1.last_name;
