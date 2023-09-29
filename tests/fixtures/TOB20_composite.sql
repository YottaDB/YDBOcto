#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB20 : OCTO959 : Test that ORDER BY on a KEY column with LIMIT on a huge table is optimized

-- Test that ORDER BY optimization works for COMPOSITE table as long as only a subset of key columns is specified in order
select * from composite order by id0 desc limit 1;
select * from composite order by id0 desc, id1 desc limit 2;

-- Test that ORDER BY optimization is disabled for COMPOSITE table if subset of key columns is specified out of order
select * from composite order by id1 desc limit 2;

