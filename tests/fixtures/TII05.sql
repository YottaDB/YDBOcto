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

-- TII05 : OCTO502 : Test various errors in INSERT INTO

SELECT '-- Test ERR_INSERT_TYPE_MISMATCH error';
INSERT INTO names SELECT firstname FROM names;
INSERT INTO names SELECT lastname FROM names;
INSERT INTO names SELECT firstname,id+6 FROM names;
INSERT INTO names SELECT firstname,firstname,lastname FROM names;
INSERT INTO names SELECT id+6,id,lastname FROM names;
INSERT INTO names SELECT id+6,firstname,TRUE from names;
INSERT INTO names SELECT (id+6)::BOOLEAN,firstname,lastname from names;
INSERT INTO names SELECT id::BOOLEAN,firstname,lastname from names;
INSERT INTO names(firstname,id) SELECT id,firstname FROM names;
INSERT INTO names(firstname,id,lastname) SELECT * FROM names;

SELECT '-- Test ERR_INSERT_TOO_MANY_EXPRESSIONS error';
INSERT INTO names SELECT id,firstname,lastname,id FROM names;
INSERT INTO names SELECT NULL,NULL,NULL,NULL FROM names;
INSERT INTO names(id) SELECT id,firstname FROM names;
INSERT INTO names(id) SELECT id,firstname FROM names;
INSERT INTO names(id,firstname) SELECT id,firstname,id FROM names;

SELECT '-- Test ERR_INSERT_TOO_MANY_COLUMNS error';
INSERT INTO names(id,firstname) SELECT id FROM names;

SELECT '-- Test ERR_TABLE_UNKNOWN_COLUMN_NAME error';
INSERT INTO names(invalid) SELECT * FROM names;
INSERT INTO names(firstname,invalid) SELECT * FROM names;
INSERT INTO names(id,lastname,invalid) SELECT * FROM names;

SELECT '-- Test that only ERR_TABLE_UNKNOWN_COLUMN_NAME error is issued if ERR_DUPLICATE_COLUMN error also exists';
INSERT INTO names(invalid,invalid) SELECT * FROM names;

SELECT '-- Test ERR_DUPLICATE_COLUMN error';
INSERT INTO names(id,invalid,id) SELECT * FROM names;
INSERT INTO names(id,firstname,firstname,lastname) SELECT * FROM names;
INSERT INTO names(id,firstname,firstname,firstname,lastname) SELECT * FROM names;

SELECT '-- Simple ERR_VARCHAR_TOO_LONG error test case';
CREATE TABLE test1 (id INTEGER PRIMARY KEY, column1 VARCHAR(3));
INSERT INTO test1 VALUES (1,'abcd');		-- 4 ascii characters
INSERT INTO test1 VALUES (2,'ＡＢＣＤ');	-- 4 utf-8 characters
SELECT * FROM test1;
DROP TABLE test1;

SELECT '-- Fancy ERR_VARCHAR_TOO_LONG error test case';
SELECT '-- Test that VARCHAR(4) does not allow 7 character strings to be stored if not all last 3 characters are spaces.';
CREATE TABLE test1 (column1 VARCHAR(4));
INSERT INTO test1 VALUES ('abcd  e');
INSERT INTO test1 VALUES ('abcd e ');
INSERT INTO test1 VALUES ('abcde  ');
INSERT INTO test1 VALUES ('ＡＢＣＤ  Ｅ');
INSERT INTO test1 VALUES ('ＡＢＣＤ Ｅ ');
INSERT INTO test1 VALUES ('ＡＢＣＤＥ  ');
SELECT * FROM test1;
DROP TABLE test1;

SELECT '-- Fancier ERR_VARCHAR_TOO_LONG error test case';
SELECT '-- Test that an over length value when typecast to varchar(N) will be truncated to N characters';
SELECT '-- And that and an error will be issued if N is over length compared to the maximum column size';
CREATE TABLE test1 (column1 character(4));
INSERT INTO test1 SELECT '|' || 'abcd'::varchar(3) || '|';
INSERT INTO test1 SELECT '|' || 'ＡＢＣＤ'::varchar(3) || '|';
SELECT * FROM test1;
DROP TABLE test1;

SELECT '-- Test of ERR_NUMERIC_OVERFLOW';
CREATE TABLE test1 (column1 NUMERIC(2,1));
INSERT INTO test1 VALUES (10);
INSERT INTO test1 VALUES (-10);
INSERT INTO test1 VALUES (9.99);
INSERT INTO test1 VALUES (-9.99);
DROP TABLE test1;

