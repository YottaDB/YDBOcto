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

-- TAF03 : OCTO517 : HAVING clause with COUNT() function usage returns incorrect results in some cases

SELECT COUNT(*) FROM names n1, names n2 HAVING COUNT(*) < 6;
SELECT COUNT(*) FROM names n1, names n2 HAVING COUNT(*) < 3;
SELECT COUNT(*) FROM names n1, names n2 HAVING COUNT(*) > 3;

SELECT COUNT(n1.firstname) FROM names n1, names n2 HAVING COUNT(n1.firstname) < 6;
SELECT COUNT(ALL n1.firstname) FROM names n1, names n2 HAVING COUNT(ALL n1.firstname) < 6;
SELECT COUNT(DISTINCT n1.firstname) FROM names n1, names n2 HAVING COUNT(DISTINCT n1.firstname) < 6;
SELECT COUNT(n1.id) FROM names n1, names n2 HAVING COUNT(n1.id) < 6;
SELECT COUNT(ALL n1.id) FROM names n1, names n2 HAVING COUNT(ALL n1.id) < 6;
SELECT COUNT(DISTINCT n1.id) FROM names n1, names n2 HAVING COUNT(DISTINCT n1.id) < 6;
SELECT AVG(n1.id) FROM names n1, names n2 HAVING AVG(n1.id) < 6;
SELECT AVG(ALL n1.id) FROM names n1, names n2 HAVING AVG(ALL n1.id) < 6;
SELECT AVG(DISTINCT n1.id) FROM names n1, names n2 HAVING AVG(DISTINCT n1.id) < 6;
SELECT SUM(n1.id) FROM names n1, names n2 HAVING SUM(n1.id) < 6;
SELECT SUM(ALL n1.id) FROM names n1, names n2 HAVING SUM(ALL n1.id) < 6;
SELECT SUM(DISTINCT n1.id) FROM names n1, names n2 HAVING SUM(DISTINCT n1.id) < 6;
SELECT MIN(n1.firstname) FROM names n1, names n2 HAVING MIN(n1.firstname) < 'Lord';
SELECT MIN(ALL n1.firstname) FROM names n1, names n2 HAVING MIN(ALL n1.firstname) < 'Lord';
SELECT MIN(DISTINCT n1.firstname) FROM names n1, names n2 HAVING MIN(DISTINCT n1.firstname) < 'Lord';
SELECT MIN(n1.id) FROM names n1, names n2 HAVING MIN(n1.id) < 6;
SELECT MIN(ALL n1.id) FROM names n1, names n2 HAVING MIN(ALL n1.id) < 6;
SELECT MIN(DISTINCT n1.id) FROM names n1, names n2 HAVING MIN(DISTINCT n1.id) < 6;
SELECT MAX(n1.firstname) FROM names n1, names n2 HAVING MAX(n1.firstname) < 'Lord';
SELECT MAX(ALL n1.firstname) FROM names n1, names n2 HAVING MAX(ALL n1.firstname) < 'Lord';
SELECT MAX(DISTINCT n1.firstname) FROM names n1, names n2 HAVING MAX(DISTINCT n1.firstname) < 'Lord';
SELECT MAX(n1.id) FROM names n1, names n2 HAVING MAX(n1.id) < 6;
SELECT MAX(ALL n1.id) FROM names n1, names n2 HAVING MAX(ALL n1.id) < 6;
SELECT MAX(DISTINCT n1.id) FROM names n1, names n2 HAVING MAX(DISTINCT n1.id) < 6;

SELECT COUNT(n1.id) FROM names n1, names n2 HAVING COUNT(n1.id) < 3;
SELECT COUNT(ALL n1.id) FROM names n1, names n2 HAVING COUNT(ALL n1.id) < 3;
SELECT COUNT(DISTINCT n1.id) FROM names n1, names n2 HAVING COUNT(DISTINCT n1.id) < 3;
SELECT AVG(n1.id) FROM names n1, names n2 HAVING AVG(n1.id) < 3;
SELECT AVG(ALL n1.id) FROM names n1, names n2 HAVING AVG(ALL n1.id) < 3;
SELECT AVG(DISTINCT n1.id) FROM names n1, names n2 HAVING AVG(DISTINCT n1.id) < 3;
SELECT SUM(n1.id) FROM names n1, names n2 HAVING SUM(n1.id) < 3;
SELECT SUM(ALL n1.id) FROM names n1, names n2 HAVING SUM(ALL n1.id) < 3;
SELECT SUM(DISTINCT n1.id) FROM names n1, names n2 HAVING SUM(DISTINCT n1.id) < 3;
SELECT MIN(n1.id) FROM names n1, names n2 HAVING MIN(n1.id) < 3;
SELECT MIN(ALL n1.id) FROM names n1, names n2 HAVING MIN(ALL n1.id) < 3;
SELECT MIN(DISTINCT n1.id) FROM names n1, names n2 HAVING MIN(DISTINCT n1.id) < 3;
SELECT MAX(n1.id) FROM names n1, names n2 HAVING MAX(n1.id) < 3;
SELECT MAX(ALL n1.id) FROM names n1, names n2 HAVING MAX(ALL n1.id) < 3;
SELECT MAX(DISTINCT n1.id) FROM names n1, names n2 HAVING MAX(DISTINCT n1.id) < 3;

SELECT COUNT(n1.id) FROM names n1, names n2 HAVING COUNT(n1.id) > 3;
SELECT COUNT(ALL n1.id) FROM names n1, names n2 HAVING COUNT(ALL n1.id) > 3;
SELECT COUNT(DISTINCT n1.id) FROM names n1, names n2 HAVING COUNT(DISTINCT n1.id) > 3;
SELECT AVG(n1.id) FROM names n1, names n2 HAVING AVG(n1.id) > 3;
SELECT AVG(ALL n1.id) FROM names n1, names n2 HAVING AVG(ALL n1.id) > 3;
SELECT AVG(DISTINCT n1.id) FROM names n1, names n2 HAVING AVG(DISTINCT n1.id) > 3;
SELECT SUM(n1.id) FROM names n1, names n2 HAVING SUM(n1.id) > 3;
SELECT SUM(ALL n1.id) FROM names n1, names n2 HAVING SUM(ALL n1.id) > 3;
SELECT SUM(DISTINCT n1.id) FROM names n1, names n2 HAVING SUM(DISTINCT n1.id) > 3;
SELECT MIN(n1.id) FROM names n1, names n2 HAVING MIN(n1.id) > 3;
SELECT MIN(ALL n1.id) FROM names n1, names n2 HAVING MIN(ALL n1.id) > 3;
SELECT MIN(DISTINCT n1.id) FROM names n1, names n2 HAVING MIN(DISTINCT n1.id) > 3;
SELECT MAX(n1.id) FROM names n1, names n2 HAVING MAX(n1.id) > 3;
SELECT MAX(ALL n1.id) FROM names n1, names n2 HAVING MAX(ALL n1.id) > 3;
SELECT MAX(DISTINCT n1.id) FROM names n1, names n2 HAVING MAX(DISTINCT n1.id) > 3;

