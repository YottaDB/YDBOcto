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

-- TCTC006 : OCTO772 : Test \d tablename with too long table and/or column names resulting in too long CHECK constraint names

-- This is to test that auto assigned constraint names take as much of the table name and column names as possible
-- The below queries experiment with different combinations of too long table and/or column names.
-- TCTC006.ref verifies that the output of the below queries in Octo matches that of Postgres.

create table TABLE_abcdefghijklmnopqrstuvwxyz (COLUMN_abcdefghijklmnopqrstuvwxyz integer
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz < 2));
\d TABLE_abcdefghijklmnopqrstuvwxyz;
drop table TABLE_abcdefghijklmnopqrstuvwxyz;

create table TABLE_abcdefghijklmnopqrstuvwxyz (COLUMN_abcdefghijklmnopqrstuvwxyz integer
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz < 2)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 0));
\d TABLE_abcdefghijklmnopqrstuvwxyz;
drop table TABLE_abcdefghijklmnopqrstuvwxyz;

create table TABLE_abcdefghijklmnopqrstuvwxyz (COLUMN_abcdefghijklmnopqrstuvwxyz integer
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 2)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 3)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 4)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 5)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 6)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 7)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 8)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 9)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 0)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 1)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 0));
\d TABLE_abcdefghijklmnopqrstuvwxyz;
drop table TABLE_abcdefghijklmnopqrstuvwxyz;

create table TABLENAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz (
	COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy integer
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 2)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 3)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 4)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 5)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 6)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 7)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 8)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 9)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 0)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 1)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 0));
\d TABLENAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz;
drop table TABLENAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz;

create table TABLENAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz (
	COLUMN_abcdefghijklmnopqrstuvwxyz integer
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 2)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 3)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 4)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 5)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 6)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 7)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 8)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 9)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 0)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 1)
	CHECK (COLUMN_abcdefghijklmnopqrstuvwxyz > 0));
\d TABLENAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz;
drop table TABLENAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz;

create table TABLENAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz (
	COLUMN_abcdef integer
	CHECK (COLUMN_abcdef > 2)
	CHECK (COLUMN_abcdef > 3)
	CHECK (COLUMN_abcdef > 4)
	CHECK (COLUMN_abcdef > 5)
	CHECK (COLUMN_abcdef > 6)
	CHECK (COLUMN_abcdef > 7)
	CHECK (COLUMN_abcdef > 8)
	CHECK (COLUMN_abcdef > 9)
	CHECK (COLUMN_abcdef > 0)
	CHECK (COLUMN_abcdef > 1)
	CHECK (COLUMN_abcdef > 0));
\d TABLENAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz;
drop table TABLENAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxyz;

create table TABLE_abcdef (
	COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy integer
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 2)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 3)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 4)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 5)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 6)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 7)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 8)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 9)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 0)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 1)
	CHECK (COLUMNNAME_abcdefghijklmnopqrstuvwxyz_abcdefghijklmnopqrstuvwxy > 0));
\d TABLE_abcdef;
drop table TABLE_abcdef;

