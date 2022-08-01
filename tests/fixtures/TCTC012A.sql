#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TCTC012 : OCTO582 : Various User Level UNIQUE Tests
-- A: Column UNIQUE unnamed constraint NOT NULL Test (most common on Github)
DROP TABLE IF EXISTS user1;
CREATE TABLE user1 (
	     id int,
	     username varchar(20) UNIQUE NOT NULL,
	     email varchar(100) UNIQUE NOT NULL,
	     pass varchar(64) NOT NULL
);
-- These two will succeed
SELECT XECUTE_M_CODE('zwrite ^%ydboctoU2tuomEAySubezrd0DoxaEI');
INSERT INTO user1 VALUES(1,'sam','sam@zzz.com','catdog.33');
INSERT INTO user1 VALUES(3,'bam','bam@zzz.com','catdog.33');
-- The following two will fail
INSERT INTO user1 VALUES(2,'sam','sam@zzz.com','catdog.33');
INSERT INTO user1 VALUES(4,'sam','','');
SELECT * FROM user1;
SELECT XECUTE_M_CODE('zwrite ^%ydboctoU2tuomEAySubezrd0DoxaEI');
UPDATE user1 SET email = 'sam@zzz.com' where id = 3;
SELECT XECUTE_M_CODE('zwrite ^%ydboctoU2tuomEAySubezrd0DoxaEI');
DELETE FROM user1 WHERE id = 3;
SELECT XECUTE_M_CODE('zwrite ^%ydboctoU2tuomEAySubezrd0DoxaEI');
\d user1;
DROP TABLE user1;

