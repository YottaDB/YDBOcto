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

