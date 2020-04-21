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

-- TC012 : OCTO484 : PIECE numbers of non-primary-key columns specified in CREATE TABLE should start at 1 by default

-- Note: Each `SELECT * FROM SIMPLE` query below is preceded by a comment line (e.g. ` -- SIMPLE0 QUERY01`).
--	This is done intentionally so later we can check which generated plan corresponds to which query.
--	The line begins with ` --` (a space) instead of `--` to avoid load_fixture() from filtering out these comment lines.

-- Test table with 0 non-primary-key columns
CREATE TABLE SIMPLE0 (id integer primary key);
 -- SIMPLE0 QUERY01
SELECT * from SIMPLE0;

-- Test table with 1 non-primary-key column
CREATE TABLE SIMPLE1 (id integer primary key, firstname varchar);
 -- SIMPLE1 QUERY02
SELECT * from SIMPLE1;
CREATE TABLE SIMPLE1 (id integer primary key, firstname varchar PIECE 1);
 -- SIMPLE1 QUERY03 (should not show up because it is same as QUERY02)
SELECT * from SIMPLE1;
CREATE TABLE SIMPLE1 (id integer primary key, firstname varchar PIECE 2);
 -- SIMPLE1 QUERY04
SELECT * from SIMPLE1;
CREATE TABLE SIMPLE1 (id integer primary key, firstname varchar PIECE 0);
 -- SIMPLE1 QUERY05
SELECT * from SIMPLE1;

-- Test table with 2 non-primary-key columns
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar, lastname varchar);
 -- SIMPLE2 QUERY06
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar, lastname varchar PIECE 0);
 -- SIMPLE2 QUERY07
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar, lastname varchar PIECE 1);
 -- SIMPLE2 QUERY08
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar, lastname varchar PIECE 2);
 -- SIMPLE2 QUERY09 (should not show up because it is same as QUERY06)
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar, lastname varchar PIECE 3);
 -- SIMPLE2 QUERY10
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 0, lastname varchar);
 -- SIMPLE2 QUERY11
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 0, lastname varchar PIECE 0);
 -- SIMPLE2 QUERY12
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 0, lastname varchar PIECE 1);
 -- SIMPLE2 QUERY13 (should not show up because it is same as QUERY11)
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 0, lastname varchar PIECE 2);
 -- SIMPLE2 QUERY14
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 0, lastname varchar PIECE 3);
 -- SIMPLE2 QUERY15
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 1, lastname varchar);
 -- SIMPLE2 QUERY16 (should not show up because it is same as QUERY06)
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 1, lastname varchar PIECE 0);
 -- SIMPLE2 QUERY17 (should not show up because it is same as QUERY07)
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 1, lastname varchar PIECE 1);
 -- SIMPLE2 QUERY18 (should not show up because it is same as QUERY08)
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 1, lastname varchar PIECE 2);
 -- SIMPLE2 QUERY19 (should not show up because it is same as QUERY09)
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 1, lastname varchar PIECE 3);
 -- SIMPLE2 QUERY20 (should not show up because it is same as QUERY10)
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 2, lastname varchar);
 -- SIMPLE2 QUERY21
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 2, lastname varchar PIECE 0);
 -- SIMPLE2 QUERY22
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 2, lastname varchar PIECE 1);
 -- SIMPLE2 QUERY23 (should not show up because it is same as QUERY21)
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 2, lastname varchar PIECE 2);
 -- SIMPLE2 QUERY24
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 2, lastname varchar PIECE 3);
 -- SIMPLE2 QUERY25
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 3, lastname varchar);
 -- SIMPLE2 QUERY26
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 3, lastname varchar PIECE 0);
 -- SIMPLE2 QUERY27
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 3, lastname varchar PIECE 1);
 -- SIMPLE2 QUERY28 (should not show up because it is same as QUERY26)
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 3, lastname varchar PIECE 2);
 -- SIMPLE2 QUERY29
SELECT * from SIMPLE2;
CREATE TABLE SIMPLE2 (id integer primary key, firstname varchar PIECE 3, lastname varchar PIECE 3);
 -- SIMPLE2 QUERY30
SELECT * from SIMPLE2;

