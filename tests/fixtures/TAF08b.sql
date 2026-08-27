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

-- The ^ORDST questions from TAF08.sql that can disagree, re-asked after a change to ^ORDST.
SELECT XECUTE_M_CODE('zwrite ^ORDST');
SELECT * FROM ordstat ORDER BY ien;
SELECT MIN(name), MAX(name) FROM ordstat;
SELECT MIN(name) FROM ordstat WHERE name IS NOT NULL;
SELECT MAX(name) FROM ordstat WHERE name IS NOT NULL;
