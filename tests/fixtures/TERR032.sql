#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR032 : OCTO502 : Test of ERR_READWRITE_DISALLOWED error

-- Test of READWRITE with table-level GLOBAL
CREATE TABLE abcd (id INTEGER) GLOBAL "^names(keys(""id""),1)" READWRITE;

-- Test of READWRITE with column-level EXTRACT
CREATE TABLE abcd (id INTEGER PRIMARY KEY, datetime INTEGER EXTRACT "$ZHOROLOG") READWRITE;

-- Test of READWRITE with column-level PIECE
CREATE TABLE abcd (id INTEGER, firstname VARCHAR(20) PIECE 3) READWRITE;

-- Test of READWRITE with column-level GLOBAL
CREATE TABLE abcd (id INTEGER, firstname VARCHAR(20) GLOBAL "^names(keys(""id""))") READWRITE;

-- Test of READWRITE with column-level DELIM
CREATE TABLE abcd (id INTEGER, firstname VARCHAR(20) DELIM "#") READWRITE;

-- Test of READWRITE with column-level START
CREATE TABLE abcd (id INTEGER START 0) READWRITE;

-- Test of READWRITE with column-level STARTINCLUDE
CREATE TABLE abcd (id INTEGER STARTINCLUDE) READWRITE;

-- Test of READWRITE with column-level END
CREATE TABLE abcd (id INTEGER END 100) READWRITE;

