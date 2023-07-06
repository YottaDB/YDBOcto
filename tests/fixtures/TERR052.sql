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

-- TERR052 : OCTO929 : Test ERR_VALUES_NOT_ALLOWED_IN_START_END error

CREATE TABLE TERR052 (keycol1 INTEGER PRIMARY KEY START 0 END "'(keys(""keycol1""))!(values(""keycol2"")="""")", keycol2 INTEGER);

