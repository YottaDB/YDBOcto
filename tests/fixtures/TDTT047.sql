#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select time(zut)'1699472687014554'; -- -2208986112985446
select date(zut)'1699472687014554'; -- skips date info 1699401600000000
select timestamp(zut)'1699472687014554'; -- 1699490687014554 -- 11-08-2023 19:44:47.014554
select date(zut)'1699419600014554'; -- skips date info 1699401600000000 -- 11-08-2023
select timestamp(zut)'1699478986721215'; -- 1699478986721215 -- 11-08-2023 21:29:46.721215
select time(zut)'1699479303057600'; -- 16:35:03.057600 -- -2208997496942400
select timestamp(zut)'1699479303057600'; -- 11-08-2023 21:35:03.057600 -- 1699479303057600
select timestamp(zut)'1699478986721215'; -- 11-08-2023 21:29:46.721215 -- 1699478986721215
select timestamp(zut)'-1'; -- 12-31-1969 23:59:59.999999 -- -1
select timestamp(zut)'-2554279907000810'; -- 01-21-1889 13:48:12.99919

create table test (id int, dot time);
create table test1 (id int, dot time with time zone);
create table test2(id int, dot timestamp with time zone);

insert into test(values(1,time'01:01:01'));
insert into test1(values(1,time with time zone'01:01:01'));
insert into test2(values(1,timestamp with time zone'01-01-2023 01:01:01'));

select * from test;
select * from test1;
select * from test2;
