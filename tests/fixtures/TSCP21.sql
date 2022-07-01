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

select 'test' NOT IN ((select NULL),NULL) from (VALUES('test1') ,('test2') ,('test3')) n1;
select 'test' NOT IN ((select * from (select NULL)n1),'test') from (VALUES('test1') ,('test2') ,('test3')) n1;
select 1 in ((select 1), 2);
select 1 in (2,(select 1), 3);
select 1 in (2,(select 2), 3);
select 'test' NOT IN ((select n1.firstname),NULL) from names n1;
select 'test' NOT IN ((select n1.lastname),(select n1.firstname)) from names n1;
