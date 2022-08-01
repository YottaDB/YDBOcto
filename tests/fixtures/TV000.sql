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

# Should issue an unknown table error
select * from nonexistanttable;

# Should NOT issue an unimplemented feature warning
INDEX fake_index "^notaglobal(""fake_index"",fields(""name""),fields(""age""),keys(""ID""))";

# Should issue no errors
select * from names;
