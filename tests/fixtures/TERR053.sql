#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR053 : OCTO1109 : Test ERR_VALUES_NOT_ALLOWED_IN_START_END error triggered via SKIPCONDITION

CREATE TABLE TERR053 (keycol1 INTEGER PRIMARY KEY SKIPCONDITION "values(""keycol2"")=""skip""", keycol2 INTEGER);

