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

-- TDFT02 : OCTO54 : Test of DELETE FROM in composite database (multiple primary key columns)

DELETE FROM composite where name = 'Name9';
SELECT * FROM composite;
DELETE FROM composite where id1 > 1;
SELECT * FROM composite;
DELETE FROM composite where id7 = 8;
SELECT * FROM composite;
DELETE FROM composite;
SELECT * FROM composite;

