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
-- Run after ^ppaddr(1) is updated directly (the table is READONLY). The AIM trigger re-derives the
-- chained value, so the index tracks the change: '45 Oak' is now gone and '999 New St' is found.
SELECT * FROM ppaddr WHERE street = '45 Oak';
SELECT * FROM ppaddr WHERE street = '999 New St';
