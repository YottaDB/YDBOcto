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

create table dater (id integer primary key , dob date) global "^datetimedate" readonly;
select * from dater;
select * from dater where dob is null;
select * from dater where dob=NULL;
select * from dater where dob=date'2023-01-01';
