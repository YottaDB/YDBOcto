#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC032 : OCTO527/OCTO320 : DDLs using EXTRACT wrap \$E[XTRACT] calls with piecevalue2colvalue

CREATE TABLE `INDEX_DESCRIPTION`(
	`INDEX_ID` NUMERIC START 0 END "'(keys(""index_id""))!(keys(""index_id"")="""")",
	`INDEX_DESCRIPTION_ID` NUMERIC START 0 END "'(keys(""index_description_id""))!(keys(""index_description_id"")="""")",
	`DESCRIPTION` CHARACTER(80) GLOBAL "^DD(""IX"",keys(""index_id""),.1,keys(""index_description_id""),0)" EXTRACT "$E($G(^DD(""IX"",keys(""index_id""),.1,keys(""index_description_id""),0)),1,245)",
	PRIMARY KEY (`INDEX_ID`, `INDEX_DESCRIPTION_ID`)
)
GLOBAL "^DD(""IX"",keys(""index_id""),.1,keys(""index_description_id""))"
DELIM "^";

select index_id from index_description;
select index_description_id from index_description;
select description from index_description;

