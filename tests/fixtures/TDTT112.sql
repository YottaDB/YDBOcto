#################################################################
#								#
# Copyright (c) 2025 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TDTT112: OCTO382 : Verify that zhorolog -> timestamp conversion for microseconds happens correctly"
SELECT timestamp(zhorolog)'47117,0,1,';
SELECT timestamp'1970-01-01 00:00:00.1';
SELECT timestamp'1970-01-01 00:00:00.000001';
