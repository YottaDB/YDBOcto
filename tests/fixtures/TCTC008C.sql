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

-- TCTC008C : Test NOT NULL constraint is checked ahead of CHECK constraint
DROP TABLE IF EXISTS domains;
CREATE TABLE domains (
  domain_id integer PRIMARY KEY,
  domain varchar(255) UNIQUE NOT NULL,
  uid int NOT NULL CHECK(uid BETWEEN 1 AND 65535),
  gid int NOT NULL CHECK(gid BETWEEN 1 AND 65535)
);
INSERT INTO domains VALUES (1,NULL,99999,99999);
