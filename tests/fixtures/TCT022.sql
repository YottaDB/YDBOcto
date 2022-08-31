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

select (select '1'||'2'):: integer;
select '12'::integer;
select 2::bool;
select 1::bool;
select 0::bool;
select 't'::VARCHAR;
select 'f'::VARCHAR;
select 1.0::VARCHAR;
select '1'::integer;
select '1'::bool;
select '0'::bool;
select 1.0::integer;
select NULL::integer;
select NULL::numeric;
select NULL::varchar;
select (select -1)::bool;

select ' +1.1'::numeric;
select '+1'::integer;
select '-1'::numeric;
select '-1.1'::numeric;
select '+1.1'::numeric;
select ' +1.1'::numeric;
select ' +1.1 '::numeric;
select ' -1.1 '::numeric;
select '+1.1 '::numeric;
select '012'::integer;
select '0012'::integer;
select '+12'::integer;
select '-12'::integer;
select ' 12'::integer;
select '12 '::integer;
select '1.'::numeric;
select '.1'::numeric;
select '10.0'::numeric(3);

-- ::numeric(precision), ::numeric(precision,scale) and varchar(precision)
select 11.23::numeric(2);
select 11.53::numeric(2);
select '11.53'::numeric(2);
select NULL::numeric(2);

select 9.56::numeric(2,1);
select 9.56::numeric(2,1);
select '9.56'::numeric(2,1);
select NULL::numeric(2,1);

select 'abcdddddd'::varchar(2);
select 12345::varchar(2);
select 123.1234::varchar(2);
select NULL::varchar(2);
select FALSE::varchar(2);

-- Following tests basic valid conversions between literal types
select NULL::integer;
select 1::integer;
select 1.1::integer;
select '1'::integer;
select FALSE::integer;

select NULL::boolean;
select 1::boolean;
select '1'::boolean;
select FALSE::boolean;


select NULL::numeric;
select 1::numeric;
select 1.1::numeric;
select '1'::numeric;

select NULL::varchar;
select 1::varchar;
select 1.1::varchar;
select '1'::varchar;
select FALSE::varchar;
