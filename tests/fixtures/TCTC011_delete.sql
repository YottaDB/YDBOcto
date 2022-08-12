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

-- TCTC011 : OCTO582 : Test col(i) usages are optimized in generated M code for UNIQUE constraint in DELETE command

create table tmp (id1 integer, id2 integer, UNIQUE(id1), UNIQUE(id2), UNIQUE(id1, id2), UNIQUE(id2, id1));
delete from tmp where id1 + id2 = 5;

