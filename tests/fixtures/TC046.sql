
#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC046 : OCTO502 : Allow READWRITE table with column-level DELIM of "" if there is only 1 non-primary-key column
-- Also test that PIECE number (specified as "PIECE 5" below) is ignored when DELIM "" is specified.

CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR DELIM "" PIECE 5) READWRITE;
-- Insert values where firstName column has default delimiter "|" in it and verify those show up in the column value
-- instead of getting filtered out (which they would if a $piece was done to extract the column value).
INSERT INTO tmp VALUES (1, 'first|1');
INSERT INTO tmp VALUES (2, 'second|2');
SELECT * FROM tmp;
SELECT firstName from tmp;
DROP TABLE tmp;

-- Test that having just one non-key column automatically adds the DELIM "" and that output is same as before.
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR) READWRITE;
INSERT INTO tmp VALUES (1, 'first|1');
INSERT INTO tmp VALUES (2, 'second|2');
SELECT * FROM tmp;
SELECT firstName from tmp;
DROP TABLE tmp;

