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

-- The same single query as TAF08c.sql, but against the STARTINCLUDE table, so the two plans can be
-- compared directly. Same global, same cross reference, same aggregates - the only difference in
-- the DDL is STARTINCLUDE, so any difference in the plan is what that keyword costs.
SELECT MIN(lo), MAX(hi) FROM hdrinc;
