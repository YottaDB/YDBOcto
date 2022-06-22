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

-- TC065 : OCTO633 : String literals in EXTRACT SQL function calls can contain only a double-quote character

drop table if exists tmp;
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30), namequote varchar extract concat (firstname, '"')) GLOBAL "^names";
select * from tmp;
