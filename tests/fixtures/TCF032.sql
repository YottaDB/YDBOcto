#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF032 : OCTO816 : Octo does not issue incorrect `ERR_FUNCTION_NOT_UNIQUE` error
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1094#note_968288462 for more background.
CREATE FUNCTION secondvalue(varchar, integer) RETURNS integer AS $$b5^TCF031;
select secondvalue(NULL, 1);
