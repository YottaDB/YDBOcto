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

-- TC067 : OCTO633 : Circular dependencies in/across EXTRACT columns is prohibited

drop table if exists tmp;
create table tmp (id integer primary key, firstname varchar, lastname varchar, fullname1 varchar extract concat(firstname, fullname2), fullname2 varchar extract concat(fullname3, lastname), fullname3 varchar extract concat(fullname1, firstname)) GLOBAL "^names";
select * from tmp;

drop table if exists tmp;
create table tmp (id integer primary key, firstname varchar, lastname varchar, fullname1 varchar extract concat(firstname, fullname2), fullname2 varchar extract concat(fullname3, lastname), fullname3 varchar extract concat(fullname4, lastname), fullname4 varchar extract concat(fullname1, firstname)) GLOBAL "^names";
select * from tmp;

drop table if exists tmp;
create table tmp (id integer primary key, firstname varchar, lastname varchar, fullname1 varchar extract concat(firstname, fullname1)) GLOBAL "^names";
select * from tmp;

drop table if exists tmp;
create table tmp (id integer primary key, firstname varchar, lastname varchar, id2 integer extract abs(id2)) GLOBAL "^names";
