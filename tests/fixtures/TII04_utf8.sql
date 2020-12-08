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

-- TII04 : OCTO502 : Simple INSERT INTO queries work

-- This file is an extension of TII04.sql.
--
-- This has queries that contain UTF-8 characters and therefore require ydb_chset to be set to UTF-8
-- in order for these to run fine in Octo.
--
-- While TII04.sql queries can be executed when ydb_chset is M or UTF-8,
-- TII04_utf8.sql queries can only be run when ydb_chset is UTF-8.

SELECT '';
SELECT '-- Test that VARCHAR(4) allows 4 characters (even if it is more than 4 bytes) to be stored.';
CREATE TABLE testu1 (column1 character(4));
INSERT INTO testu1 VALUES ('ＡＢＣＤ');
SELECT column1 FROM testu1;
DROP TABLE testu1;

SELECT '';
SELECT '-- Test that VARCHAR(4) allows character strings shorter than 4 chars to be stored without space padding at end';
CREATE TABLE testu2 (column1 character(4));
INSERT INTO testu2 VALUES ('ＡＢＣ');
INSERT INTO testu2 VALUES ('ＡＢ');
INSERT INTO testu2 VALUES ('Ａ');
SELECT '|' || column1 || '|' FROM testu2;
DROP TABLE testu2;

SELECT '';
SELECT '-- Test that VARCHAR(4) allows 7 character strings to be stored if last 3 characters are spaces.';
CREATE TABLE testu3 (column1 character(4));
INSERT INTO testu3 VALUES ('ＡＢＣＤ   ');
INSERT INTO testu3 VALUES ('ＡＢＣ    ');
SELECT '|' || column1 || '|' FROM testu3;
DROP TABLE testu3;

SELECT '';
SELECT '-- Test that an over length value when typecast to varchar(N) will be truncated to N characters without an error.';
CREATE TABLE testu4 (column1 character(4));
INSERT INTO testu4 SELECT '|' || 'ＡＢＣＤ'::varchar(2) || '|';
SELECT * FROM testu4;
DROP TABLE testu4;

