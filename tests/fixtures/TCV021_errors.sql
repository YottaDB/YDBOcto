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

create view v1 as select 1;
create view v1 as select 2;

drop view v1;
create table v1 (id INTEGER);
create view v1 as select 1;

drop table v1;
create view v1 as select 1;
create table v1 (id INTEGER);
create table if not exists v1 (id INTEGER);
