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

create table tdtt037 (tob time(6));
insert into tdtt037 values(time'01:01:01.999999');
select * from tdtt037;
drop table tdtt037;

create table tdtt0371 (tsob timestamp(6));
insert into tdtt0371 values(timestamp'2023-01-01 01:01:01.999999');
select * from tdtt0371;
drop table tdtt0371;

create table tdtt037a (tob time(6) without time zone);
insert into tdtt037a values(time'01:01:01.999999');
select * from tdtt037a;
drop tabel tdtt037a;

create table tdtt0371a (tsob timestamp(6) without time zone);
insert into tdtt0371a values(timestamp'2023-01-01 01:01:01.999999');
select * from tdtt0371a;
drop table tdtt0371a;

create table tdtt0372 (tob time(6) with time zone);
insert into tdtt0372 values(time with time zone'01:01:01.999999');
select * from tdtt0372;
drop table tdtt0372;

create table tdtt0373 (tsob timestamp(6) with time zone);
insert into tdtt0373 values(timestamp with time zone'2023-01-01 01:01:01.999999');
select * from tdtt0373;
drop table tdtt0373;

create table tdtt0374 (tob time(10));
create table tdtt0375 (tsob timestamp(10));
drop table tdtt0374;
drop table tdtt0375;

select timestamp'2023-01-01 01:01:01.0001';
select timestamp'2023-01-01 01:01:01.01';
select time'01:01:01.001';
