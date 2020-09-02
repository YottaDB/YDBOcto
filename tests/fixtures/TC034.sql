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

-- TC034 : OCTO527/OCTO320 : DDLs using NULLCHAR and EXTRACT wrap \$E[XTRACT] calls with empty2null

CREATE TABLE `INDEX_DESCRIPTION`(
	`INDEX_ID` NUMERIC PRIMARY KEY START 0 END "'(keys(""INDEX_ID""))!(keys(""INDEX_ID"")="""")",
	`INDEX_DESCRIPTION_ID` NUMERIC KEY NUM 1 START 0 END "'(keys(""INDEX_DESCRIPTION_ID""))!(keys(""INDEX_DESCRIPTION_ID"")="""")",
	`DESCRIPTION` CHARACTER(80) GLOBAL "^DD(""IX"",keys(""INDEX_ID""),.1,keys(""INDEX_DESCRIPTION_ID""),0)" EXTRACT "$E($G(^DD(""IX"",keys(""INDEX_ID""),.1,keys(""INDEX_DESCRIPTION_ID""),0)),1,245)"
)
GLOBAL "^DD(""IX"",keys(""INDEX_ID""),.1,keys(""INDEX_DESCRIPTION_ID""))"
NULLCHAR (127)
DELIM "^";

select index_id from index_description;
select index_description_id from index_description;
select description from index_description;

