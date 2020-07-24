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
-- TCT015 : OCTO475 : Multiple usages of the :: (typecast) operator should be accepted
select 'abcd'::varchar;
select 'abcd'::varchar::varchar;
select ('abcd'::varchar)::varchar;
select 'abcd'::varchar::varchar::varchar::varchar;
select 1::boolean::integer::numeric::text;
select true::integer::numeric;
select 'a'::text::varchar;
select 1.64::integer::boolean::text;
