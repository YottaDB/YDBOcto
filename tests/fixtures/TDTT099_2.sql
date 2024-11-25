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
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1611#note_2254023516
select timestamp_to_zut(timestamp'1969-12-31 23:59:59.111');
select timestamp(zut)'-889000';
