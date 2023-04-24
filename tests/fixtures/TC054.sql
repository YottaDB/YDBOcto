#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC054 : OCTO931 : Test no assert failure in CREATE TABLE when multi-column UNIQUE constraint and READONLY is used

-- We expect a `ERR_UNKNOWN_COLUMN_NAME` error below, not an assert failure (when run with a Debug build of Octo).
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, UNIQUE (id2, id1)) GLOBAL "^x(keys(""%YO_KEYCOL""))" READONLY;

-- We expect a `ERR_GLOBAL_MISSING_KEY_COLS` error below, not an assert failure (when run with a Debug build of Octo).
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, UNIQUE (id2, id1)) GLOBAL "^x(keys(""ID1""))" READONLY;

-- We expect a `ERR_READONLY_DISALLOWED` error below, not an assert failure (when run with a Debug build of Octo).
CREATE TABLE TMP (id1 INTEGER, id2 INTEGER, UNIQUE (id2, id1)) GLOBAL "^x(keys(""ID1""),keys(""ID2"")))" READONLY;

