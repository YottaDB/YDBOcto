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

-- The ^HDR questions from TAF08d.sql that can disagree, re-asked after a change to ^HDR.
SELECT XECUTE_M_CODE('zwrite ^HDR');

-- hdr : ien 0 is NOT a row.
SELECT * FROM hdr ORDER BY ien;
SELECT MIN(lo), MAX(lo) FROM hdr;
SELECT MIN(hi), MAX(hi) FROM hdr;
SELECT MIN(lo) FROM hdr WHERE lo IS NOT NULL;
SELECT MAX(hi) FROM hdr WHERE hi IS NOT NULL;

-- hdrinc : same global, but ien 0 IS a row.
SELECT * FROM hdrinc ORDER BY ien;
SELECT MIN(lo), MAX(lo) FROM hdrinc;
SELECT MIN(hi), MAX(hi) FROM hdrinc;
SELECT MIN(lo) FROM hdrinc WHERE lo IS NOT NULL;
SELECT MAX(hi) FROM hdrinc WHERE hi IS NOT NULL;
