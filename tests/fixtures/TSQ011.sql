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

-- TSQ011 : function lookup query succeeds without error

select proname,typname from pg_catalog.pg_proc inner join pg_catalog.pg_type on pg_catalog.pg_proc.prorettype = pg_catalog.pg_type.oid;
