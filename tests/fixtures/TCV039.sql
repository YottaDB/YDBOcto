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

create view v1 as select 't';
select * from v1;
\d v1;

create view v2 as select '1';
select * from v2;
\d v2;

create view v3 as select 'f';
select * from v3;
\d v3;

create view v4 as select '0';
select * from v4;
\d v4;

create view v5 as select true;
select * from v5;
\d v5;

create view v6 as select false;
select * from v6;
\d v6;

create view v7 as select true,'t' as col2 union select 'f','t';
select * from v7;
\d v7;

create view v8 as select '1'::integer from names limit 1;
select * from v8;
\d v8;

create view v9 as select '1'::integer;
select * from v9;
\d v9;
