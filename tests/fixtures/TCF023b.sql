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

-- This is invoked from Stage 2 of the TCF023 subtest

-- TCF023 : OCTO90 : Rerunning query after CREATE FUNCTION should recreate plans that relied on the recreated function

DROP FUNCTION REPLACEF(VARCHAR, VARCHAR, VARCHAR);
CREATE FUNCTION REPLACEF(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$^%ydboctoreplace;

