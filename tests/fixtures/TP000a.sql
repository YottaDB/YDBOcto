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

-- TP000 : OCTO656 : INSERT, UPDATE, DELETE prohibited if table is READONLY, rocto run with -w, and user can readwrite

drop table if exists TP000;
create table TP000 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30)) READONLY;
