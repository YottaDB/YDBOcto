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
create table timestamp_fileman_tbl (order_id integer primary key, order_timestamp timestamp(fileman)) GLOBAL "^timestampfilemantbl" READONLY;
select order_timestamp from timestamp_fileman_tbl where order_timestamp=timestamp(fileman)'3111208.171839';
select * from timestamp_fileman_tbl;
