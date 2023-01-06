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

CREATE TABLE nullnames (id INTEGER PRIMARY KEY NOT NULL, firstName VARCHAR(30), lastName VARCHAR(30), salary NUMERIC(20,2) NOT NULL, exempt BOOLEAN NOT NULL, yearsTenured INTEGER) GLOBAL "^nullnames";
CREATE TABLE nullnamesb (id INTEGER PRIMARY KEY NOT NULL, firstName VARCHAR(30), lastName VARCHAR(30), salary NUMERIC(20,2) NOT NULL, exempt BOOLEAN NOT NULL, yearsTenured INTEGER NOT NULL) GLOBAL "^nullnamesb";

