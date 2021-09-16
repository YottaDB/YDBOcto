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

-- TVA0002 : Test INSERT INTO, DELETE FROM queries against ORDER1 table are disallowed due to READ_ONLY

INSERT INTO ORDER1(ORDER1_ID) VALUES(-1);
DELETE FROM ORDER1;

