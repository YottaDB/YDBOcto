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

-- TC076 : OCTO685 : Check error issued if column is computed (EXTRACT) and key column (KEY NUM) at same time

-- Test of ERR_EXTRACT_CANNOT_BE_KEY_COLUMN error
-- Test of PRIMARY KEY and EXTRACT
CREATE TABLE tmp (id INTEGER PRIMARY KEY EXTRACT "$ZDATE($HOROLOG,""YEAR-MM-DD 24:60:SS"")") GLOBAL "^tmp";

-- Test of ERR_EXTRACT_CANNOT_BE_KEY_COLUMN error
-- Test of KEY NUM and EXTRACT
CREATE TABLE tmp (id INTEGER PRIMARY KEY, datetime INTEGER EXTRACT "$ZDATE($HOROLOG,""YEAR-MM-DD 24:60:SS"")" KEY NUM 1) GLOBAL "^tmp";

