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

create view v1 as select 1,'Zero','Cool'; -- Error as all columns have "?column?" as its name
create view v1 as select 1 as n1_id,'Zero' as n1_firstname, 'Cool' as n1_firstname; -- ERROR:  column "n1_firstname" specified more than once
