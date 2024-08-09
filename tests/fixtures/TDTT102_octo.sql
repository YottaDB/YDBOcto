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

create table tdtt102timestamp(id int primary key, dob timestamp) global "^tdtt102timestamp(keys(""id""))" readonly;
select * from tdtt102timestamp;
create table tdtt102timestamptz(id int primary key, dob timestamp with time zone) global "^tdtt102timestamptz(keys(""id""))" readonly;
select * from tdtt102timestamptz;
