#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC058 : OCTO519 : Case-sensitivity of double-quoted table names is preserved in constraint names

drop table if exists "T";
drop table if exists "t";
drop table if exists tmp;
drop table if exists tmp2;
drop table if exists tmp3;
