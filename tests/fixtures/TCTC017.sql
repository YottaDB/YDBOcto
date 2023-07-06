#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCTC017 : OCTO929 : Test that UNIQUE constraint global names are unchanged after YDBOcto#929

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/929#note_1508425327

create table "TCTC017a" (id integer primary key, id1 integer, id2 integer, UNIQUE (id1, id2));
\d "TCTC017a";

create table TCTC017b (id integer primary key, id1 integer, id2 integer, UNIQUE (id1, id2));
\d TCTC017b;

create table "TCTC017c" (id integer primary key, "iD1" integer, id2 integer, UNIQUE ("iD1", id2));
\d "TCTC017a";

create table TCTC017d (id integer primary key, id1 integer, "iD2" integer, UNIQUE (id1, "iD2"));
\d TCTC017b;

