#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB06 : OCTO228 : Support ORDER BY with more than one column
-- Note: We use MODULO here instead of % operator since we want to also test function call usages with parameters
CREATE FUNCTION MODULO(INTEGER, INTEGER) RETURNS INTEGER AS $$^MODULO;

select id as c1, id as c2 from names order by c1,c2;
select id as c1, id as c2 from names order by c2,c1;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c2,c3;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c2 asc,c3;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c2,c3 asc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c2 desc,c3;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c2,c3 desc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c2 asc,c3 asc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c2 asc,c3 desc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c2 desc,c3 asc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c2 desc,c3 desc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c3,c2;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c3 asc,c2;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c3,c2 asc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c3 desc,c2;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c3,c2 desc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c3 asc,c2 asc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c3 asc,c2 desc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c3 desc,c2 asc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3 from names order by c3 desc,c2 desc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3, MODULO(id,2) as c4 from names order by c2,c3,c4;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3, MODULO(id,2) as c4 from names order by c3,c4,c2;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3, MODULO(id,2) as c4 from names order by c4,c2,c3;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3, MODULO(id,2) as c4 from names order by c4 desc,c2 asc,c3 desc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3, MODULO(id,2) as c4 from names order by c4 asc,c2 desc,c3 desc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3, MODULO(id,2) as c4 from names order by c4 asc,c2 desc,c3 asc;
select id as c1, MODULO(id,2) as c2, MODULO(id,3) as c3, MODULO(id,2) as c4, MODULO(id,3) as c5 from names order by c2 desc,c3 asc,c4 desc,c5 desc;
select id as c1, MODULO(id,2) as c2, MODULO(id,4) as c3, MODULO(id,2) as c4, MODULO(id,3) as c5 from names order by c2 desc,c3 asc,c4 desc,c5 desc;
select id as c1, MODULO(id,3) as c2, MODULO(id,4) as c3, MODULO(id,2) as c4, MODULO(id,3) as c5 from names order by c2 desc,c3 asc,c4 desc,c5 desc;
select id as c1, MODULO(id,3) as c2, MODULO(id,4) as c3, MODULO(id,3) as c4, MODULO(id,3) as c5 from names order by c2 desc,c3 asc,c4 desc,c5 desc;
select id as c1, MODULO(id,3) as c2, MODULO(id,4) as c3, MODULO(id,3) as c4, MODULO(id,4) as c5 from names order by c2 desc,c3 asc,c4 desc,c5 desc;
