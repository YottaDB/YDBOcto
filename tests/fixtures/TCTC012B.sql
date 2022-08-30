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
-- TCTC012 : OCTO582 : Various User Level UNIQUE Tests
-- B: Table UNIQUE named constraint on multiple NOT NULL fields
DROP TABLE IF EXISTS subscriptions;
CREATE TABLE subscriptions (
	   uid              INT NOT NULL,
	   user_id          INT NOT NULL,
	   service_id       VARCHAR(64)  NOT NULL,
	   url              VARCHAR(512) NOT NULL,
	   --CONSTRAINT pk_subscriptions PRIMARY KEY (uid),
	   CONSTRAINT unique_subscriptions UNIQUE (user_id, service_id, url)
);
SELECT XECUTE_M_CODE("zwrite ^%ydboctoUAmZMXJ5Co5UNpwPQfk5TL5");
INSERT INTO subscriptions VALUES
(1, 1, 'aa', 'http://foo.com'),
(3, 1, 'cc', 'http://foo.com');
SELECT * FROM subscriptions;
SELECT XECUTE_M_CODE("zwrite ^%ydboctoUAmZMXJ5Co5UNpwPQfk5TL5");
-- These two will fail
INSERT INTO subscriptions VALUES (2, 1, 'aa', 'http://foo.com');
UPDATE subscriptions SET service_id = 'aa' WHERE uid = 3;
SELECT * FROM subscriptions;
SELECT XECUTE_M_CODE("zwrite ^%ydboctoUAmZMXJ5Co5UNpwPQfk5TL5");
\d subscriptions;
DROP TABLE IF EXISTS subscriptions;
