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

-- TC026 : OCTO527 : CREATE TABLE accepts user-defined NULL character

CREATE TABLE nullcharnames (id INTEGER PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30)) NULLCHAR (127) GLOBAL "^nullcharnames(keys(""id""))";
