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

-- TCK04 : OCTO860 : RIGHT/FULL JOIN on tables with composite keys returns correct results

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/860#description
SELECT DISTINCT composite.id4 FROM composite  RIGHT JOIN composite AS alias1 ON (((composite.name = alias1.name))) WHERE (((composite.id6) IS NOT NULL) OR (NOT ((composite.name) IS NULL)));

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/860#note_1021085147
SELECT * FROM composite c1 RIGHT JOIN composite c2 ON (c1.name = c2.name) WHERE (c1.id6 = 6);

-- Simplest test of YDBOcto#860
SELECT * FROM composite c1 RIGHT JOIN composite c2 ON (c2.name > 'Name8');

-- Test FULL JOIN with the above queries
SELECT DISTINCT composite.id4 FROM composite  FULL JOIN composite AS alias1 ON (((composite.name = alias1.name))) WHERE (((composite.id6) IS NOT NULL) OR (NOT ((composite.name) IS NULL)));
SELECT * FROM composite c1 FULL JOIN composite c2 ON (c1.name = c2.name) WHERE (c1.id6 = 6);
-- The below query issues a 'FULL JOIN is only supported with merge-joinable or hash-joinable join conditions' error in Postgres
-- but works in Octo and hence is currently commented out since cross-check requires both to run it fine.
-- SELECT * FROM composite c1 FULL JOIN composite c2 ON (c2.name > 'Name8');

