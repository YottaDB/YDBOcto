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

create function samevalue(integer) returns integer as $$samevalue^functions;
create view v1 as select samevalue(1);
select * from v1;
-- Expect the following error for the drop function statement
-- ERROR:  cannot drop function samevalue(integer) because other objects depend on it
-- DETAIL:  view v1 depends on function samevalue(integer)
-- HINT:  Use DROP ... CASCADE to drop the dependent objects too.
drop function samevalue(integer);
