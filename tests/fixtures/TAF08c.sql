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

-- One query on its own, so that exactly one M plan is generated and can be shown. What the plan
-- must make visible is that MIN/MAX here really do read the cross reference's value dimension --
-- a $ORDER on the AIM global rather than a FOR loop over the table's keys -- because that is the
-- whole subject of this test. Anything the plan does about START is visible here too.
SELECT MIN(lo), MAX(hi) FROM hdr;
