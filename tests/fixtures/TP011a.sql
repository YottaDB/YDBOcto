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

-- TP011 : OCTO656 : Only allowschemachanges permissions granted if rocto run with -a and user has readwrite+allowschemachanges permissions

drop table if exists TP001;
create table TP001 (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30));
