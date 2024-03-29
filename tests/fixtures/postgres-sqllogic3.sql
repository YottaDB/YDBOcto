-- ######################################################################
-- #									#
-- # Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

/* This file was autogenerated with the following commands:
num=3
schema=sqllogic$num
fixtures=$(git rev-parse --show-toplevel)/tests/fixtures
$fixtures/sqllogic/insert.py $num
# Remove GLOBAL subexpressions from CREATE TABLE statements, since postgres does not understand them.
sed 's/ GLOBAL .*;$/;/' $schema.sql > $fixtures/postgres-$schema.sql
# Remove "DROP TABLE IF EXISTS" line if it exists
*/

CREATE TABLE s3t1(id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, c INTEGER, d INTEGER, e INTEGER)
;
INSERT INTO s3t1(id,e,c,b,d,a) VALUES(0,NULL,102,NULL,101,104);
INSERT INTO s3t1(id,a,c,d,e,b) VALUES(1,107,106,108,109,105);
INSERT INTO s3t1(id,e,d,b,a,c) VALUES(2,110,114,112,NULL,113);
INSERT INTO s3t1(id,d,c,e,a,b) VALUES(3,116,119,117,115,NULL);
INSERT INTO s3t1(id,c,d,b,e,a) VALUES(4,123,122,124,NULL,121);
INSERT INTO s3t1(id,a,d,b,e,c) VALUES(5,127,128,129,126,125);
INSERT INTO s3t1(id,e,c,a,d,b) VALUES(6,132,134,131,133,130);
INSERT INTO s3t1(id,a,d,b,e,c) VALUES(7,138,136,139,135,137);
INSERT INTO s3t1(id,e,c,d,a,b) VALUES(8,144,141,140,142,143);
INSERT INTO s3t1(id,b,a,e,d,c) VALUES(9,145,149,146,NULL,147);
INSERT INTO s3t1(id,b,c,a,d,e) VALUES(10,151,150,153,NULL,NULL);
INSERT INTO s3t1(id,c,e,a,d,b) VALUES(11,155,157,159,NULL,158);
INSERT INTO s3t1(id,c,b,a,d,e) VALUES(12,161,160,163,164,162);
INSERT INTO s3t1(id,b,d,a,e,c) VALUES(13,167,NULL,168,165,166);
INSERT INTO s3t1(id,d,b,c,e,a) VALUES(14,171,170,172,173,174);
INSERT INTO s3t1(id,e,c,a,d,b) VALUES(15,177,176,179,NULL,175);
INSERT INTO s3t1(id,b,e,a,d,c) VALUES(16,181,180,182,183,184);
INSERT INTO s3t1(id,c,a,b,e,d) VALUES(17,187,188,186,189,185);
INSERT INTO s3t1(id,d,b,c,e,a) VALUES(18,190,194,193,192,191);
INSERT INTO s3t1(id,a,e,b,d,c) VALUES(19,199,197,198,196,195);
INSERT INTO s3t1(id,b,c,d,a,e) VALUES(20,NULL,202,203,201,204);
INSERT INTO s3t1(id,c,e,a,b,d) VALUES(21,208,NULL,NULL,206,207);
INSERT INTO s3t1(id,c,e,a,d,b) VALUES(22,214,210,213,212,211);
INSERT INTO s3t1(id,b,c,a,d,e) VALUES(23,218,215,216,217,219);
INSERT INTO s3t1(id,b,e,d,a,c) VALUES(24,223,221,222,220,224);
INSERT INTO s3t1(id,d,e,b,a,c) VALUES(25,226,227,228,229,225);
INSERT INTO s3t1(id,a,c,b,e,d) VALUES(26,234,231,232,230,233);
INSERT INTO s3t1(id,e,b,a,c,d) VALUES(27,237,236,239,NULL,238);
INSERT INTO s3t1(id,e,c,b,a,d) VALUES(28,NULL,244,240,243,NULL);
INSERT INTO s3t1(id,e,d,c,b,a) VALUES(29,246,248,247,249,245);
