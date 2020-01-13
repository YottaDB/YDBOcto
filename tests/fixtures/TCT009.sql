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

-- OCTO395 : Assertion failed in LP_VERIFY_STRUCTURE.C when UNARY + operator is used

select * from names where id = 2 || 3;
select * from names where id = 2 || (+3);
select * from names where '23' = 2 || 3;
select * from names where '2-3' = 2 || -3;
select * from names where '23' = 2 || (+3);
select * from names where firstname = +'abcd';
select * from names where firstname = +'Zero';
select * from names where firstname = +lastname;
select * from names where id = +3;
select * from names where id = -(-(+3));
select * from names where id = 2+(+3);
select * from names where firstname = 'Zero' || +'';
select * from names where firstname = 'Zero' || -'';
select * from names where firstname = 'Zero' || +lastname;
select * from names where firstname = 'Zero' || -lastname;

-- Note: The below queries currently work in Octo but fail with Postgres.
--       The Octo output might change once YDBOcto#304 is fixed

select * from names where firstname = 'Zero' || lastname::integer;
select * from names where firstname = 'Zero' || 'abcd'::integer;
select * from names where firstname = 'Zero' || ''::integer;
select * from names where firstname = 'Zero' or lastname::integer;
select * from names where firstname = 'Zero' or 'abcd'::integer;
select * from names where firstname = 'Zero' or ''::integer;

