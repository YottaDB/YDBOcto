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

drop table if exists tdtt055;
create table TDTT055 (id int, tob time);
insert into TDTT055 values(1, time with time zone'11:00:00-05:00');
insert into TDTT055 values(1, timestamp with time zone'2023-01-01 11:00:00-05:00');
insert into TDTT055 values(1, timestamp'2023-01-01 11:00:00');
insert into tdtt055 values(1,time'01:01:01');

drop table if exists tdtt055;
create table TDTT055 (id int, dob timestamp with time zone);
insert into TDTT055 values(1,date'2023-01-01');
insert into TDTT055 values(1,timestamp'2023-01-01 11:00:00');
insert into TDTT055 values(1, timestamp with time zone'2023-01-01 11:00:00-05:00');

drop table if exists tdtt055;
create table tdtt055 (id int, dob date);
insert into tdtt055 values(1,timestamp'2023-01-01 01:01:01');
insert into tdtt055 values(1,timestamp with time zone'2023-01-01 01:01:01');
insert into TDTT055 values(1,date'2023-01-01');

drop table if exists tdtt055;
create table tdtt055 (id int, dob timestamp);
insert into tdtt055 values(1,date'2023-01-01');
insert into tdtt055 values(1,timestamp with time zone'2023-01-01 01:01:01');
insert into tdtt055 values(1,timestamp'2023-01-01 01:01:01');

drop table if exists tdtt055;
create table tdtt055 (id int, tob time with time zone);
insert into tdtt055 values(1,time'01:01:01');
insert into tdtt055 values(1,time with time zone'01:01:01');

drop table if exists dt;
drop table if exists tmstmp;
CREATE TABLE dt (order_date DATE);
CREATE TABLE tmstmp (order_timestamp TIMESTAMP);
insert into tmstmp values (timestamp '7208-07-11 10:16:27');
insert into dt select order_timestamp from tmstmp;
select * from dt;
select * from tmstmp;
