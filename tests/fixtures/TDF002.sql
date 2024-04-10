#################################################################
#								#
# Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- This is invoked from Stage 2 of the TDF002 subtest

-- TDF002 : OCTO90 : DROP FUNCTION should delete db nodes for plans that relied on the dropped function

DROP FUNCTION REPLACEF (VARCHAR, VARCHAR, VARCHAR);

