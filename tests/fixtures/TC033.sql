#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC033 : OCTO524 :  Maintain text based table definition of each column in sync with CREATE TABLE specification" {

CREATE TABLE names1 (id INTEGER(32) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(57));
CREATE TABLE names2 (id NUMERIC(16) PRIMARY KEY, firstName VARCHAR(17), lastName TEXT(31));
CREATE TABLE names3 (id NUMERIC(13,7) PRIMARY KEY, firstName VARCHAR(37), lastName TEXT(26));
CREATE TABLE datetime1 (id INTEGER PRIMARY KEY, date1 DATE, time1 TIME(19));

-- This is a fancy query that tests DELIM/START/END/KEY NUM etc.
CREATE TABLE fancy (
 KEYCOL1 INTEGER PRIMARY KEY UNIQUE START 0 END "'(keys(""KEYCOL1""))!(keys(""KEYCOL1"")="""")",
 KEYCOL2 INTEGER KEY NUM 1 START 0 END "'(keys(""KEYCOL2""))!('keys(""KEYCOL1""))",
 NONKEYCOL1 CHARACTER(64) NOT NULL GLOBAL "^XMB(1,keys(""KEYCOL1""),3.2,keys(""KEYCOL2""),0)" PIECE 17 DELIM (64),
 NONKEYCOL2 CHARACTER(21) GLOBAL "^ABCD" EXTRACT "$E($G(^XMB(""KEYCOL1"")),1,245)" PIECE 38 DELIM "#"
)
GLOBAL "^XMB(1,keys(""keycol1""),3.2,keys(""keycol2""))"
DELIM (67,68);

