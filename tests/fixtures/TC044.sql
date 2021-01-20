
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
-- TC044 : OCTO575 : Octo CREATE TABLE naively creates invalid GVNs from table names
CREATE TABLE information_schema.testeroni (tester VARCHAR);
SELECT * FROM information_schema.testeroni;
