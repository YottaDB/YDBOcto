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

-- The column type for this in Postgres is `text`
create view v4 as select '';
\d v4;
-- Following queries can't be run with Postgres cross check as JDBC client outputs `null` in Octo and empty row in Postgres(even through the row printed is empty there is a null value here)
select * from v4;

-- The column type for this in Postgres is `text`
create view v5 as select NULL;
\d v5;

-- The column type for this in Postgres is `text`
create view v6 as select array(select '');
\d v6;
-- Following queries can't be run with Postgres cross check as JDBC client outputs `null` in Octo and empty row in Postgres(even through the row printed is empty there is a null value here)
select * from v6;

-- The column type for this in Postgres is `text`
create view v7 as select array(select '' from names);
\d v7;
-- Following queries can't be run with Postgres cross check as JDBC client outputs `null` in Octo and empty row in Postgres(even through the row printed is empty there is a null value here)
select * from v7;

-- The column type for this in Postgres is `text`
create view v8 as select array(select NULL);
\d v8;

-- The column type for this in Postgres is `text`
create view v9 as select array(select NULL from names);
\d v9;

