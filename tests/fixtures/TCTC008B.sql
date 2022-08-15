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

-- TCTC008B : Test NOT NULL constraint is allowed and enforced on a READWRITE type table + various NULL behavior tests

-- Test that NOT NULL constraint is allowed on a READWRITE type table
-- Test that NOT NULL constraint can specify a name
DROP TABLE IF EXISTS articles;
CREATE TABLE articles(
article_id INTEGER CONSTRAINT article_id_pk PRIMARY KEY,
category_id INTEGER CONSTRAINT article_category_nn NOT NULL
); -- implicitly READWRITE

-- Test that \d tablename does NOT display constraint names for NOT NULL but
-- instead just displays whether the column has "not null" specified or not.
-- Just like Postgres \d tablename does.
\d articles;

-- Populate some data for UPDATE
INSERT INTO articles
VALUES
(3,5),
(8,10);
SELECT * FROM articles;

-- Test that NOT NULL constraint is enforced on a READWRITE type table
-- Test INSERT INTO where column is explicitly specified and takes on a NULL value
INSERT INTO articles(article_id, category_id)
VALUES
(6,NULL);

-- Test INSERT INTO where column is not explicitly specified but takes on a NULL value
INSERT INTO articles(article_id)
VALUES
(6);

-- Test UPDATE where column is explicitly specified and takes on a NULL value
UPDATE articles SET category_id = NULL WHERE article_id = 3;
SELECT * FROM articles;
