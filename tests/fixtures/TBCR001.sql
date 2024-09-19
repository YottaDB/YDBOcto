#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TBCR001: OCTO1058: Sanity checks using octo
-- Normal use case
BEGIN;
SELECT COUNT(*) FROM names;
COMMIT;

-- Alternate 1 (end instead of commit)
BEGIN;
SELECT COUNT(*) FROM names;
END;

-- Alternate 2 (rollback instead of commit)
BEGIN;
SELECT COUNT(*) FROM names;
ROLLBACK;

-- Test error scenarios
-- ERR_TRANSACTION_IN_PROGRESS
BEGIN;
BEGIN;
END;

-- ERR_NO_TRANSACTION_IN_PROGRESS
ROLLBACK;
COMMIT;

-- ERR_TRANSACTION_NO_UPDATES
BEGIN;
INSERT INTO names values (6, 'foo', 'boo');
UPDATE names SET lastname='foo' WHERE id=1;
DELETE FROM names WHERE id=1;

DISCARD ALL;
DISCARD XREFS;

TRUNCATE names;

DROP TABLE IF EXISTS names2;
CREATE TABLE names2 (id INTEGER PRIMARY KEY, name VARCHAR(6));

DROP VIEW IF EXISTS namesview;
CREATE VIEW namesview as SELECT * FROM names;

DROP FUNCTION IF EXISTS ABS2(INTEGER);
CREATE FUNCTION ABS2(INTEGER) RETURNS NUMERIC AS $$ABS^%ydboctosqlfunctions;

--Not in Octo yet...
--DROP INDEX IF EXISTS namesindex;
--CREATE INDEX namesindex;

--Run select and make sure that still works
-- Test "case table_alias_STATEMENT:" in src/run_query.c
SELECT COUNT(*) FROM names;

-- Test "case set_operation_STATEMENT:" in src/run_query.c
SELECT COUNT(*) FROM names UNION select id from names where firstname = 'Lord';

-- Test "case display_relation_STATEMENT:" in src/run_query.c
\d;        -- Test "case DISPLAY_ALL_RELATION"
\d names;  -- Test "case DISPLAY_TABLE_RELATION"
\dv;       -- Test "case DISPLAY_ALL_VIEW_RELATION"

COMMIT;
