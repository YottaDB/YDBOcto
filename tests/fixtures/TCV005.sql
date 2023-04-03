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

create view TCV005_1v1 as select id,firstname,lastname from names;
select TCV005_1v1.id,TCV005_1v1.firstname,TCV005_1v1.lastname from TCV005_1v1;
drop view TCV005_1v1;
