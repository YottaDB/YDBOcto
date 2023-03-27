#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDT07 : OCTO735 : DROP TABLE with KEEPDATA keyword does not do a KILL of the underlying GVN storing the table data

DROP TABLE names KEEPDATA;
DROP TABLE nameswithages KEEPDATA;
