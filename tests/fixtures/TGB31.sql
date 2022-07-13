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

-- Below test queries are from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/850#description
SELECT 1 FROM names x HAVING EXISTS(SELECT t1.id, MAX(t1.lastName) FROM names t1 LEFT JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id having 2 IN (SELECT t1.id FROM names y ORDER BY t2.*,t1.id limit 1));
-- Below 2 queries are commented out because they need YDBOcto#870 to also be fixed (not just YDBOcto#850).
-- SELECT 1 FROM names x HAVING EXISTS(SELECT t1.id, MAX(t1.lastName) FROM names t1 LEFT JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id having 0 IN (SELECT t1.id FROM names y ORDER BY t2.*,t1.id limit 1));
-- SELECT 1 FROM names x HAVING EXISTS(SELECT t1.id, MAX(t1.lastName) FROM names t1 LEFT JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id having 1 IN (SELECT t1.id FROM names y ORDER BY t2.*,t1.id limit 1));

-- Below test queries are simplified versions of the above (from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/850#description)
SELECT 1 FROM names x HAVING EXISTS(SELECT 1 FROM names t1 LEFT JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE t2.lastName IS NULL);
SELECT 1 FROM names x HAVING EXISTS(SELECT 1 FROM names t1 INNER JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE t2.lastName IS NULL);

-- Below test queries are from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/850#note_1025637531
SELECT 1 WHERE  EXISTS(SELECT 1 FROM names INNER JOIN (SELECT id FROM names) t2 ON TRUE WHERE t2.id = 2);

