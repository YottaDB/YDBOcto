#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select 1 from names where count(1) = 1;
select 1 where count(1) = 1;
select 1 where max(1) = 1;
select 1 where max(1+1) = 2;
select 1 where max(1 - 1 + 1 / 1 * (select 1)) = 1;
select 1 where max((select id from names limit 1))=1;
select 1 where max('test'||'testa')='test';
