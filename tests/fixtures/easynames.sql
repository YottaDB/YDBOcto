#################################################################
#								#
# Copyright (c) 2019-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- there is an easy mapping from id to name. for id = 1 the name is Name1, for id = 2 name is Name2 etc.

CREATE TABLE easynames (id INTEGER PRIMARY KEY, name VARCHAR(6)) GLOBAL "^easynames";

