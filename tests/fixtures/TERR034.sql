#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR034 : OCTO772 : Test of ERR_READONLY_DISALLOWED and ERR_READONLY_AND_READWRITE_DISALLOWED errors

-- Test of ERR_READONLY_DISALLOWED error
-- Test of CHECK constraint with READONLY explicitly specified
CREATE TABLE abcd (id INTEGER CHECK (id < 10)) READONLY;
-- Test of UNIQUE constraint with READONLY explicitly specified
CREATE TABLE abcd (id INTEGER UNIQUE) READONLY;

-- Test of ERR_READONLY_AND_READWRITE_DISALLOWED error
-- Test of CHECK constraint with READONLY implicitly assumed (due to the column-level EXTRACT keyword)
CREATE TABLE abcd (id INTEGER PRIMARY KEY CHECK (id < 10), datetime INTEGER EXTRACT "$ZHOROLOG");
CREATE TABLE abcd (id INTEGER PRIMARY KEY, datetime INTEGER EXTRACT "$ZHOROLOG" CHECK (datetime < id));

