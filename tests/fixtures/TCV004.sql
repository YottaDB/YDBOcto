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


create view TCV004_1v1 (v1_id,v1_firstname,v1_lastname) as select 1,'Zero','Cool'; -- Works because outer most column aliasing resolves the conflict noticed in the previous query
select * from TCV004_1v1;
drop view TCV004_1v1;
create view TCV004_1v1 (v1_id,v1_firstname,v1_lastname) as select 1 as n1_id,'Zero' as n1_firstname, 'Cool' as n1_firstname; -- Works because outer most column aliasing resolves the conflict
select * from TCV004_1v1;
drop view TCV004_1v1;
