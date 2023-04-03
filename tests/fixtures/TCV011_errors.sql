#################################################################
#                                                               #
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

create table test (id integer, firstname varchar, lastname varchar);
create view v1 as select * from test;
select * from v1;
-- Expect the following error from the drop table statement
-- ERROR:  cannot drop table test because other objects depend on it
-- DETAIL:  view v1 depends on table test
-- HINT:  Use DROP ... CASCADE to drop the dependent objects too.
drop table test;

