#################################################################
#                                                               #
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

select lastname from names order by lastname limit 1, firstname limit 1;
select lastname from names order by lastname limit 1, firstname, id limit 1;
select lastname from names order by lastname, firstname limit 1, id limit 1;
select * from names order by lastname limit 1, firstname;
