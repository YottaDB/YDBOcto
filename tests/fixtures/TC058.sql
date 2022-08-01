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

-- TC058 : OCTO519 : Case-sensitivity of double-quoted table names is preserved in constraint names

create table "id ABcd" ("col Tmp" integer CHECK ("col Tmp" > 2), "col TMP" integer CHECK ("col TMP" > 2));
\d "id ABcd";

create table "T" (id integer primary key);
create table "t" (id integer primary key);
\d "T";
\d "t";

create table tmp ("id 1" integer, "id 2" integer, unique("id 2", "id 1"), constraint "c 3" CHECK ("id 1" < "id 2"));
create table tmp2 (id1 integer, id2 integer, unique(id2, id1), constraint c3 CHECK (id1 < id2));
create table tmp3 ("id%1" integer);
\d tmp;
\d tmp2;
\d tmp3;
select * from tmp3;
