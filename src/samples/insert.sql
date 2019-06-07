#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

INSERT INTO myTable VALUES (1, 2, "Name");
INSERT INTO myTable (id, name) VALUES (1, "Emma");
INSERT INTO myTable (id, name) VALUES
  (1, "Emma"),
  (2, "Jojo"),
  (3, "Max")
;
INSERT INTO myTable2
SELECT * from myTable3;
INSERT INTO theTable DEFAULT VALUES;
