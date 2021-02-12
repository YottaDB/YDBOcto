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

-- TC011 : OCTO411 : Add support for INTEGER precision specification to parser for CREATE TABLE statements

-- Test that INTEGER(precision) type works
CREATE TABLE names (id INTEGER(64) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
SELECT id FROM names;
SELECT id::integer FROM names;
SELECT id::numeric FROM names;
SELECT id::varchar FROM names;
SELECT id::text FROM names;
SELECT id::date FROM names;
SELECT id::time FROM names;

-- Test that NUMERIC(precision) type works
DROP TABLE names;
CREATE TABLE names (id NUMERIC(10) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
SELECT id FROM names;
SELECT id::integer FROM names;
SELECT id::numeric FROM names;
SELECT id::varchar FROM names;
SELECT id::text FROM names;
SELECT id::date FROM names;
SELECT id::time FROM names;

-- Test that NUMERIC(precision,scale) type works
DROP TABLE names;
CREATE TABLE names (id NUMERIC(10,4) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
SELECT id FROM names;
SELECT id::integer FROM names;
SELECT id::numeric FROM names;
SELECT id::varchar FROM names;
SELECT id::text FROM names;
SELECT id::date FROM names;
SELECT id::time FROM names;

-- Test various precision and scale values with INTEGER and NUMERIC (some work, some issue error)
DROP TABLE names;
CREATE TABLE names (id INTEGER(4) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(8) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(16) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(32) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(64) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(127) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(0) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(-1) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(-1.5) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(15.5) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER('abcd') PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id INTEGER(abcd) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;

DROP TABLE names;
CREATE TABLE names (id NUMERIC(4) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(8) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(16) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(32) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(64) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(127) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(0) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(-1) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(-1.5) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(15.5) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC('abcd') PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(abcd) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;

DROP TABLE names;
CREATE TABLE names (id NUMERIC(4,2) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(8,7) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(16,18) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(32,25) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(64,2047) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(127,85.5) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(0,100.87) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(-1,20) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(-1.5,-2.5) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(15.5,20.835) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC('abcd',20.835) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC('abcd','efgh') PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(abcd,'efgh') PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;
CREATE TABLE names (id NUMERIC(abcd,efgh) PRIMARY KEY, firstName VARCHAR(30), lastName TEXT(30)) GLOBAL "^names(keys(""id""))" READONLY;
DROP TABLE names;

