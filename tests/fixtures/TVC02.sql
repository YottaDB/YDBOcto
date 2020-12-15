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

-- TVC02 : OCTO502 : Verify VALUES clause results in no unnecessary physical plans

VALUES (1, 'First');
VALUES (12, 'First', 'Last'), (13, 'First2', 'Last2');
VALUES (12, 'First', 'Last'), (13, NULL, 'Last2');
SELECT * FROM (VALUES (1, 'First')) AS n1(id, firstname);
SELECT * FROM (VALUES (1, 'First')) n1;
SELECT * FROM (VALUES (12, 'First', 'Last'), (13, 'First2', 'Last2')) n1, names n2;
SELECT n1.firstname from (VALUES (1, 'First')) AS n1(id, firstname);

