#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- target: timestamptz
-- source: timestamp,date,timestamptz
drop table if exists TDTT109;
create table TDTT109 (id int, dob timestamp with time zone);
insert into TDTT109 values(1, timestamp'2023-01-01 01:01:01');
insert into TDTT109 values(2, timestamp with time zone'2023-01-01 01:01:01+06');
insert into TDTT109 values(3, date'2023-01-01');
insert into TDTT109 values(4, date'2023-08-01');
insert into TDTT109 values(5, '2023-08-01');
insert into TDTT109 values(6, '2023-01-01');
insert into TDTT109 values(7, '2023-01-01 01:01:01');
insert into TDTT109 values(8, '2023-01-01 01:01:01+06');
select * from TDTT109;
drop table TDTT109;

-- target: timestamp
-- source: timestamptz,date
drop table if exists TDTT109;
create table TDTT109 (id int, dob timestamp);
insert into TDTT109 values(1, timestamp'2023-01-01 01:01:01');
insert into TDTT109 values(2, timestamp with time zone'2023-01-01 01:01:01+06');
insert into TDTT109 values(3, timestamp with time zone'2023-01-01 01:01:01-05');
insert into TDTT109 values(4, timestamp with time zone'2023-08-01 01:01:01-04');
insert into TDTT109 values(5, date'2023-01-01');
insert into TDTT109 values(6, date'2023-08-01');
insert into TDTT109 values(7, '2023-01-01 01:01:01');
insert into TDTT109 values(8, '2023-01-01 01:01:01+06');
insert into TDTT109 values(9, '2023-01-01 01:01:01-05');
insert into TDTT109 values(10, '2023-08-01 01:01:01-04');
insert into TDTT109 values(11, '2023-01-01');
insert into TDTT109 values(12, '2023-08-01');
select * from TDTT109;
drop table TDTT109;

-- target: date
-- source: timestamptz,timestamp
drop table if exists TDTT109;
create table TDTT109 (id int, dob date);
insert into TDTT109 values(2, timestamp with time zone'2023-01-01 01:01:01+06');
insert into TDTT109 values(3, timestamp with time zone'2023-01-01 01:01:01-05');
insert into TDTT109 values(4, timestamp with time zone'2023-08-01 01:01:01-04');
insert into TDTT109 values(1, timestamp'2023-01-01 01:01:01');
insert into TDTT109 values(5, timestamp'2023-01-01 01:01:01');
insert into TDTT109 values(6, timestamp'2023-08-01 01:01:01');
insert into TDTT109 values(7, '2023-01-01 01:01:01+06');
insert into TDTT109 values(8, '2023-01-01 01:01:01-05');
insert into TDTT109 values(9, '2023-08-01 01:01:01-04');
insert into TDTT109 values(10, '2023-01-01 01:01:01');
insert into TDTT109 values(11, '2023-01-01 01:01:01');
insert into TDTT109 values(12, '2023-08-01 01:01:01');
select * from TDTT109;
drop table TDTT109;

-- target: time
-- source: timestamptz,timestamp
drop table if exists TDTT109;
create table TDTT109 (id int, dob date);
insert into TDTT109 values(2, timestamp with time zone'2023-01-01 01:01:01+06');
insert into TDTT109 values(3, timestamp with time zone'2023-01-01 01:01:01-05');
insert into TDTT109 values(4, timestamp with time zone'2023-08-01 01:01:01-04');
insert into TDTT109 values(1, timestamp'2023-01-01 01:01:01');
insert into TDTT109 values(5, timestamp'2023-01-01 01:01:01');
insert into TDTT109 values(6, timestamp'2023-08-01 01:01:01');
insert into TDTT109 values(7, '2023-01-01 01:01:01+06');
insert into TDTT109 values(8, '2023-01-01 01:01:01-05');
insert into TDTT109 values(9, '2023-08-01 01:01:01-04');
insert into TDTT109 values(10, '2023-01-01 01:01:01');
insert into TDTT109 values(11, '2023-01-01 01:01:01');
insert into TDTT109 values(12, '2023-08-01 01:01:01');
select * from TDTT109;
drop table TDTT109;

