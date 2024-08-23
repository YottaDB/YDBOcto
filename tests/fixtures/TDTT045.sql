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

create table test (id int primary key, dob date) global "^datetime(keys(""id""))" readonly;
select dob from test limit 4;
select dob from test where dob between date'2000-01-01' and date'2100-01-01';
select dob from test where dob > date'2000-01-01' and dob < date'2100-01-01';
