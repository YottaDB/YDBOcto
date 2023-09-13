#################################################################
#                                                              #
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.      #
# All rights reserved.                                         #
#                                                              #
#      This source code contains the intellectual property     #
#      of its copyright holder(s), and is made available       #
#      under a license.  If you do not know the terms of       #
#      the license, please stop and do not read further.       #
#                                                              #
#################################################################

drop table if exists test;
create table test (COMMAND varchar);
insert into test values('test_command');
select command from test;

-- Copied from YDBOcto#1015
create table xyz (name varchar primary key, command varchar);
