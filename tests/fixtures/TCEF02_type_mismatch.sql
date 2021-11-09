#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Test of ERR_TYPE_MISMATCH error
-- Also tests : OCTO780 : ERR_TYPE_MISMATCH error in NULLIF function parameters incorrectly continues with query execution
SELECT NULLIF(1, '1');

