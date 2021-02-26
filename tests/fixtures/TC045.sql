
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

-- TC045 : OCTO502 : Test of READONLY and READWRITE keywords in CREATE TABLE

-- Test both READONLY and READWRITE specified in same CREATE TABLE command. Latest should prevail.

CREATE TABLE MYTBL1 (id INTEGER PRIMARY KEY) READONLY READONLY READWRITE;
CREATE TABLE MYTBL2 (id INTEGER PRIMARY KEY) READONLY READONLY READWRITE READWRITE READONLY;

-- Test multiple GLOBAL keyword specification. Latest keyword should prevail.
CREATE TABLE MYTBL3 (id INTEGER PRIMARY KEY) GLOBAL "^MYTBL31(keys(""id""))" GLOBAL "^MYTBL32(keys(""id""))" ;

-- Test multiple DELIM keyword specification. Latest keyword should prevail.
CREATE TABLE MYTBL4 (id INTEGER PRIMARY KEY) DELIM "@" DELIM "#";

-- Test READONLY keyword overrides any octo.conf "tabletype" setting
CREATE TABLE MYTBL6 (id INTEGER PRIMARY KEY) READONLY;

-- Test READWRITE keyword overrides any octo.conf "tabletype" setting
CREATE TABLE MYTBL7 (id INTEGER PRIMARY KEY) READWRITE;

-- Test octo.conf "tabletype" setting prevails if neither READONLY no READWRITE is specified
-- Also test that if "tabletype" setting is not specified in octo.conf, READWRITE is the default.
CREATE TABLE MYTBL8 (id INTEGER PRIMARY KEY);

-- Test that READWRITE is compatible with table-level DELIM and column-level PRIMARY KEY/KEY NUM/NOT NULL/UNIQUE
CREATE TABLE MYTBL9 (id INTEGER PRIMARY KEY, id2 INTEGER KEY NUM 1, firstname VARCHAR(10) NOT NULL UNIQUE) DELIM "#" READWRITE;

