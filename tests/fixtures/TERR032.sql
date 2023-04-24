#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
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
CREATE TABLE abcd (id INTEGER, PRIMARY KEY(id)) GLOBAL "^names(keys(""ID""),1)" READWRITE;

-- Test of READWRITE with column-level EXTRACT
CREATE TABLE abcd (id INTEGER PRIMARY KEY, datetime INTEGER EXTRACT "$ZHOROLOG") READWRITE;

-- Test of READWRITE with column-level PIECE
CREATE TABLE abcd (id INTEGER, firstname VARCHAR(20) PIECE 3) READWRITE;

-- Test of READWRITE with column-level GLOBAL
CREATE TABLE abcd (id INTEGER PRIMARY KEY, firstname VARCHAR(20) GLOBAL "^names(keys(""ID""))") READWRITE;

-- Test of READWRITE with column-level DELIM
CREATE TABLE abcd (id INTEGER, firstname VARCHAR(20) DELIM "#") READWRITE;

-- Test of READWRITE with column-level START
CREATE TABLE abcd (id INTEGER START 0) READWRITE;

-- Test of READWRITE with column-level STARTINCLUDE
CREATE TABLE abcd (id INTEGER STARTINCLUDE) READWRITE;

-- Test of READWRITE with column-level END
CREATE TABLE abcd (id INTEGER END 100) READWRITE;

-- Test of READWRITE explicitly specified and column-level DELIM that is not ""
CREATE TABLE abcd (id INTEGER, firstname VARCHAR DELIM "a") READWRITE;

-- Test of READWRITE not explicitly specified but being default and column-level DELIM that is not ""
-- In this case it might be more user-friendly to assume READONLY and not issue an error.
-- But ERR_READWRITE_DISALLOWED error is how Octo behaves currently and so we test that here.
CREATE TABLE abcd (id INTEGER, firstname VARCHAR DELIM "a");

-- Test of READWRITE not explicitly specified where multiple non-key columns with DELIM "" have been specified.
-- In this case it might be more user-friendly to assume READONLY and not issue an error.
-- But ERR_READWRITE_DISALLOWED error is how Octo behaves currently (as it assumed READWRITE to create the hidden
-- key column initially and cannot easily change that to READONLY later) and so we test that here.
CREATE TABLE abcd (id INTEGER, firstname VARCHAR DELIM "", lastname VARCHAR DELIM "");

