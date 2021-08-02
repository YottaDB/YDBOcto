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

-- TS016 : OCTO727 : Test SET operations whose operands end up as deferred physical plans

-- Note: The OR operator below tests LP_SET_DNF type of SET operations
--       Whereas operations like UNION test LP_SET_UNION type of SET operations.
--       The code handles deferred plans differently for LP_SET_DNF type and non-LP_SET_DNF types of SET operations.
--       Hence only UNION is tested in the latter category.

SELECT * FROM names n1 WHERE id IN ((SELECT id from names WHERE id = n1.id) UNION (SELECT id from names ORDER BY firstname LIMIT 1));
SELECT * FROM names n1 WHERE id IN ((SELECT id from names ORDER BY firstname LIMIT 1) UNION (SELECT id from names WHERE id = n1.id));
SELECT * FROM names n1 WHERE id IN ((SELECT id from names WHERE id = n1.id) UNION (SELECT 4) UNION (SELECT 1));
SELECT * FROM names n1 WHERE id IN ((SELECT id from names WHERE id = n1.id) UNION (SELECT 1) UNION (SELECT 4));
SELECT * FROM names n1 WHERE id IN ((SELECT 1) UNION (SELECT id from names WHERE id = n1.id) UNION (SELECT 4));
SELECT * FROM names n1 WHERE id IN ((SELECT 4) UNION (SELECT id from names WHERE id = n1.id) UNION (SELECT 1));
SELECT * FROM names n1 WHERE id IN ((SELECT 1) UNION (SELECT 4) UNION (SELECT id from names WHERE id = n1.id));
SELECT * FROM names n1 WHERE id IN ((SELECT 4) UNION (SELECT 1) UNION (SELECT id from names WHERE id = n1.id));
SELECT * FROM names n1 WHERE id IN (SELECT id FROM names WHERE n1.firstname = 'Joey' OR lastname = 'Killer');
SELECT * FROM names n1 WHERE n1.firstName IN (SELECT n1.firstName UNION SELECT 'Joey' UNION SELECT 'Acid');
SELECT * FROM names n1 WHERE n1.firstName IN (SELECT 'Joey' UNION SELECT n1.firstName UNION SELECT 'Acid');
SELECT * FROM names n1 WHERE n1.firstName IN (SELECT 'Acid' UNION SELECT n1.firstName UNION SELECT 'Joey');

-- Misc queries. The below queries were found to not work fine during an intermediate state of the fixes for YDB#727.
-- It is not clear if they are already part of an existing automated test or not.
-- Therefore adding them here just in case.

SELECT (SELECT 6-n2.id FROM names n2 where n2.id = n1.id) FROM names n1 where (n1.id <= 2 OR n1.id >= 3) ORDER BY 1;
SELECT (SELECT 1) FROM names WHERE (true OR false) ORDER BY 1;

