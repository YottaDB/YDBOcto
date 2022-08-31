#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TTA003 : OCTO759 : TABLENAME.ASTERISK in GROUP BY should not be expanded to list of table columns : Error scenarios

-- Test of ERR_GROUP_BY_OR_AGGREGATE_FUNCTION error
-- Below tests queries from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_732301265

SELECT lastname FROM names GROUP BY names.* ORDER BY firstname;
SELECT * FROM names GROUP BY names.*;
SELECT id FROM names GROUP BY names.* HAVING id < 1;
SELECT COUNT(id) FROM names GROUP BY names.* HAVING id < 1;
SELECT n1.* FROM names n1 GROUP BY n1.* HAVING n1.* IS NOT NULL;

-- Test of ERR_GROUP_BY_OR_AGGREGATE_FUNCTION error
-- Below queries are moved from TSCP14_table_asterisk.sql (note: this file was nixed as part of YDBOcto#759 changes)
-- They used to work before YDBOcto#759 code changes but issue an error (like Postgres) after then.
SELECT n1.* FROM names1col n1 GROUP BY n1.* HAVING SUM(n1.*)>1;
SELECT n1.* FROM names1col n1 GROUP BY n1.* HAVING MIN(n1.*)>1;
SELECT n1.* FROM names1col n1 GROUP BY n1.* HAVING MAX(n1.*)>1;
SELECT n1.* FROM names1col n1 GROUP BY n1.* HAVING AVG(n1.*)>1;
SELECT n1.* FROM names1col n1 GROUP BY n1.* HAVING SUM(DISTINCT n1.*)>1;
SELECT n1.* FROM names1col n1 GROUP BY n1.* HAVING MIN(DISTINCT n1.*)>1;
SELECT n1.* FROM names1col n1 GROUP BY n1.* HAVING MAX(DISTINCT n1.*)>1;
SELECT n1.* FROM names1col n1 GROUP BY n1.* HAVING AVG(DISTINCT n1.*)>1;

-- Test of ERR_GROUP_BY_OR_AGGREGATE_FUNCTION error
-- Below queries are moved from TSCP15.sql
-- They used to work before YDBOcto#759 code changes but issue an error (like Postgres) after then.
SELECT firstname FROM names GROUP BY names.*;
SELECT COUNT(n1.*),n1.customer_id FROM customers AS n1 GROUP BY n1.*;
SELECT n1.customer_id FROM customers AS n1 GROUP BY n1.*;
SELECT COUNT(n1.customer_id),n1.customer_id FROM customers AS n1 GROUP BY n1.*;
SELECT COUNT(n1.*),* FROM customers AS n1 GROUP BY n1.*;
SELECT COUNT(n1.*),n1.name FROM ((SELECT 1 AS id, 'test' AS name) UNION (SELECT 2 AS id, 'test' AS name)) AS n1 GROUP BY n1.*;
SELECT COUNT(n1.*),n1.id FROM names1col AS n1 GROUP BY n1.*;
SELECT COUNT(n1.*),n1.* FROM customers AS n1 GROUP BY n1.*;
SELECT n1.* FROM customers AS n1 GROUP BY n1.*;
SELECT n1.* FROM names n1 GROUP BY n1.*;
SELECT COUNT(n1.*),* from names1col AS n1 GROUP BY n1.*;
SELECT n1.* FROM names1col AS n1 GROUP BY n1.*;

-- Test of ERR_GROUP_BY_OR_AGGREGATE_FUNCTION error
-- Below is from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/759#note_738472680
SELECT 1 FROM names n1 GROUP BY n1.firstname HAVING n1.* IS NOT NULL;

-- Test of ERR_TYPE_CAST error
SELECT SUM(n1.*::INTEGER) FROM (SELECT id FROM names) n1;
SELECT AVG(n1.*::INTEGER) FROM (SELECT id FROM names) n1;
SELECT MAX(n1.*::INTEGER) FROM (SELECT id FROM names) n1;
SELECT MIN(n1.*::INTEGER) FROM (SELECT id FROM names) n1;
SELECT COUNT(n1.*::INTEGER) FROM (SELECT id FROM names) n1;
SELECT COUNT(DISTINCT n1.*::INTEGER) FROM (SELECT id FROM names) n1;
SELECT SUM(n1.*::NUMERIC) FROM (SELECT id FROM names) n1;
SELECT AVG(n1.*::NUMERIC) FROM (SELECT id FROM names) n1;
SELECT MAX(n1.*::NUMERIC) FROM (SELECT id FROM names) n1;
SELECT MIN(n1.*::NUMERIC) FROM (SELECT id FROM names) n1;
SELECT COUNT(n1.*::NUMERIC) FROM (SELECT id FROM names) n1;
SELECT COUNT(DISTINCT n1.*::NUMERIC) FROM (SELECT id FROM names) n1;
SELECT SUM(n1.*::VARCHAR) FROM (SELECT id FROM names) n1;
SELECT AVG(n1.*::VARCHAR) FROM (SELECT id FROM names) n1;
SELECT MAX(n1.*::VARCHAR) FROM (SELECT id FROM names) n1;
SELECT MIN(n1.*::VARCHAR) FROM (SELECT id FROM names) n1;
SELECT COUNT(n1.*::VARCHAR) FROM (SELECT id FROM names) n1;
SELECT COUNT(DISTINCT n1.*::VARCHAR) FROM (SELECT id FROM names) n1;
SELECT SUM(n1.*::BOOLEAN) FROM (SELECT id FROM names) n1;
SELECT AVG(n1.*::BOOLEAN) FROM (SELECT id FROM names) n1;
SELECT MAX(n1.*::BOOLEAN) FROM (SELECT id FROM names) n1;
SELECT MIN(n1.*::BOOLEAN) FROM (SELECT id FROM names) n1;
SELECT COUNT(n1.*::BOOLEAN) FROM (SELECT id FROM names) n1;
SELECT COUNT(DISTINCT n1.*::BOOLEAN) FROM (SELECT id FROM names) n1;

-- Test of ERR_UNKNOWN_COLUMN_NAME error when AS is used with TABLENAME.ASTERISK
SELECT n1.* AS x, n1.* AS b FROM names n1 ORDER BY a, b;

-- Test of ERR_ORDER_BY_SELECT_DISTINCT error when TABLENAME.ASTERISK is used in SELECT column list AND ORDER BY list
-- The TABLENAME.ASTERISK in the SELECT column list is expanded but the ORDER BY one is not. And so an error is expected.
-- Postgres too issues a similar error in this case.
SELECT DISTINCT n1.* FROM names n1 ORDER BY n1.*;

