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

-- TC060 : OCTO519 : Column names differing only in case yield correct constraint names

drop table if exists tmp;
create table tmp ("id" integer unique, "Id" integer unique, "iD" integer unique, id integer unique);
\d tmp;