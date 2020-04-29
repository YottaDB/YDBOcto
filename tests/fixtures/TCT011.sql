#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCT011 : OCTO304 : Type cast operator (`::`) does not work
CREATE FUNCTION DOLLARZWRITE(INTEGER) RETURNS VARCHAR AS $ZWRITE;

select id::integer,ABS(2) from names;
select id::integer,(ABS(2)::text || id::text) from names;
select id::integer,(ABS(2)::numeric + id) from names;
select id::text || id::text,ABS(2)::numeric * id from names;
select id from (select * from names UNION select 6.5::integer,'A','B');
select firstname::integer from names;
select * from names where firstname::integer = id;
select id-2*(id/2::integer) from names;
select id-2*((id/2)::integer) from names;
select DOLLARZWRITE(id)::integer from names;

-- Test typecast to boolean. Occasionally test that `bool` is equivalent to `boolean`
select id::boolean,(ABS(2)::boolean || id::bool) from names;
select id::boolean,(ABS(2)::bool + id) from names;
select id::boolean || id::boolean,ABS(2)::numeric * id from names;
select id::boolean || id::boolean,ABS(2)::boolean * id from names;
select id from (select * from names UNION select 6.5::bool,'A','B');
select * from names where firstname::boolean = id;
select id-2*(id/2::bool) from names;
select id-2*((id/2)::boolean) from names;

