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

-- column alias
-- following two queries generate syntax error from AS rule
select id as 123 from names;
select id as $1 from names;

-- following types of alias usage is not supported (parser throws error)
select id as `backtick` from names;
select id as lit.lit from names;
select id as 'quote'.'quote' from names;
select id as "quote"."quote" from names;
select id as $$dummyextrinsicfunction^something from names;

-- wrong column alias name
select idb from (select 8 as "ida");
select idb from (select 8 as 'ida');
select idb from (select 8 as ida);

-- table alias
-- following two queries generate syntax error from AS rule
select id from names as 123;
select id from names as $1;

-- following types of alias usage is not supported (parser throws error)
select id from names as `backtick`;
select id from names as lit.lit;
select id from names as 'quote'.'quote';
select id from names as "quote"."quote";
select id from names as $$dummyextrinsicfunction^something;

-- wrong table alias name
select n2.id from names as "n1";
select n2.id from names as n1;

-- shorthand alias usage --
-- column alias
-- following two queries generate syntax error from AS rule
select id 123 from names;
select id $1 from names;

-- following types of alias usage is not supported (throws error)
select id `backtick` from names;
select id lit.lit from names;
select id 'quote'.'quote' from names;
select id "quote"."quote" from names;
select id $$dummyextrinsicfunction^something from names;

-- wrong column alias name
select idb from (select 8 "ida") n1;
select idb from (select 8 'ida') n1;
select idb from (select 8 ida) n1;

-- table alias
-- following two queries generate syntax error from AS rule
select id from names 123;
select id from names $1;

-- following types of alias usage is not supported (parser throws error)
select id from names `backtick`;
select id from names lit.lit;
select id from names 'quote'.'quote';
select id from names "quote"."quote";
select id from names $$dummyextrinsicfunction^something;

-- wrong table alias name
select n2.id from names "n1";
select n2.id from names n1;

-- wrong column alias with null table alias
select idb from (select 8 "ida");
select idb from (select 8 'ida');
select idb from (select 8 ida);
