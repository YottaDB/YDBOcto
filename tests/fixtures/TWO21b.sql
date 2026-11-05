#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- ############################################################################################################
-- Part M: Part D's table, re-asked after each change to ^ANC made in M, as an application would make it. The
-- triggers maintain the cross reference of b. Its answers must agree with the table scan (the first query) after
-- every change: a row the scan finds with b = 5 or b = 6 is found through the cross reference, and no other.
SELECT XECUTE_M_CODE('zwrite ^ANC');
SELECT * FROM anc;
SELECT * FROM anc WHERE b = 5 ORDER BY a;
SELECT * FROM anc WHERE b = 6 ORDER BY a;
