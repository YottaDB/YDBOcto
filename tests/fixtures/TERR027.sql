#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR027 : OCTO345 : Issue error for extrinsic functions with '%' in invalid places

CREATE FUNCTION BADFUNC() RETURNS INTEGER AS $$BAD%FUNC;
CREATE FUNCTION BADFUNC() RETURNS INTEGER AS $$BAD^FU%NC;

