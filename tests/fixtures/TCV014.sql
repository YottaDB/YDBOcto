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

create view TCV014_1v1 as select id,lastname from names;
select * from TCV014_1v1;
drop view TCV014_1v1;
create view TCV014_1v1 as select id,firstname from names;
select * from TCV014_1v1;
drop view TCV014_1v1;
