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

select '01-01-2023 01:01:01'::TIME; -- works in postgres but not in Octo
select '01-01-2023'::TIMESTAMP;
select '01/01/2023'::TIMESTAMP; -- works in postgres but not in Octo as the input format doesn't match with config format

select '01-01-2023 01:01:01'::time;
select '01-01-2023'::timestamp;
select '01:01:01'::timestamp;
