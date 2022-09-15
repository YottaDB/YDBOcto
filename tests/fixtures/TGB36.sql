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

-- ORDER BY
select 1 from names order by count(1) = 1;
select 1 order by count(1) = 1;
select 1 order by max(1) = 1;
select 1 order by max(1+1) = 2;
select 1 order by max(1 - 1 + 1 / 1 * (select 1)) = 1;
select 1 order by max((select id from names limit 1));
select 1 order by max('test'||'testa')='test';

-- SELECT
select count(1) = 1 from names;
select count(1) = 1;
select max(1) = 1;
select max(1+1) = 2;
select max(1 - 1 + 1 / 1 * (select 1)) = 1;
select max((select id from names limit 1));
select max('test'||'testa')='test';

-- HAVING
select 1 from names having count(1) = 1;
select 1 having count(1) = 1;
select 1 having max(1) = 1;
select 1 having max(1+1) = 2;
select 1 having max(1 - 1 + 1 / 1 * (select 1)) = 1;
select 1 having max((select id from names limit 1)) = 1;
select 1 having max('test'||'testa')='test';
