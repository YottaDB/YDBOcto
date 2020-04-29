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

-- TCF022 : OCTO345 : Catalog queries on pg_proc work for full table name and alias

SELECT proname,pronargs,prorettype,proargtypes FROM pg_catalog.pg_proc;
SELECT proname,pronargs,prorettype,proargtypes FROM pg_proc;
