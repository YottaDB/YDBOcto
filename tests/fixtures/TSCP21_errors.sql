#################################################################
#                                                              #
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.      #
# All rights reserved.                                         #
#                                                              #
#      This source code contains the intellectual property     #
#      of its copyright holder(s), and is made available       #
#      under a license.  If you do not know the terms of       #
#      the license, please stop and do not read further.       #
#                                                              #
#################################################################

select 'test' NOT IN ((select n1.id),NULL) from names n1;
select 'test' NOT IN (NULL,(select n1.id)) from names n1;
select 'test' NOT IN (NULL,(select id from names)) from names n1;
select 'test' NOT IN (NULL,(select firstname from names)) from names n1;
