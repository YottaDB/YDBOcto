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
-- TITER15 : Section C : two ITERATOR tables in one plan
SELECT a.extractctx AS a_extract, b.extractctx AS b_extract, a.iterctx AS a_iter, b.iterctx AS b_iter
	FROM titer15a a, titer15b b LIMIT 3;
