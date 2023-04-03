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

create view v1 as select id, firstname,lastname from names;
create view v2 as select * from v1;
-- Expect the following from the DROP VIEW
-- ERROR:  cannot drop view v1 because other objects depend on it
-- DETAIL:  view v2 depends on view v1
-- HINT:  Use DROP ... CASCADE to drop the dependent objects too.
drop view v1;

-- Following commands shouldn't lead to any errors
drop view v2;
drop view v1;

