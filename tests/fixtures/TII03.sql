#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TII03 : INSERT INTO a 3-key table

CREATE TABLE namesXfer (
  id INTEGER PRIMARY KEY
, firstName VARCHAR(30)
, lastName VARCHAR(30)
);
INSERT INTO namesXfer (SELECT * FROM names WHERE lastName IS NOT NULL);
SELECT * FROM namesXfer;
DROP TABLE namesXfer;

