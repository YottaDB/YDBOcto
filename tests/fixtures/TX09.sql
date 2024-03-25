#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TX09 : OCTO1034 : %YDBAIM-F-BADTEMPLATE error when indexing string subscripts that include "/"
CREATE TABLE tmp (id INTEGER PRIMARY KEY, name VARCHAR GLOBAL "^tmp(keys(""id""),""/"")") GLOBAL "^tmp" AIMTYPE 1;
SELECT * FROM tmp WHERE name = 'abcd';
