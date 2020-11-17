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

-- Below queries are expected to throw a type missmatch error since CASE value and WHEN condition result types are different
select case firstname when firstname = 'Zero' then 'MyZero' else firstname end from names;
select case when 'Zero' then 'MyZero' else firstname end from names;
select case id when firstname='Zero' then 'MyZero' else firstname end from names;
select case id when firstname then 'MyZero' else firstname end from names;
select case (select 1) when firstname then 'MyZero' else firstname end from names;
select case 'Zero' when then 'MyZero' else firstname end from names;
select case when 'Zero' then 'MyZero' else 1 end from names;
select case 1 = 1 when 0 then 1 end;
select case 1 = 1 when 1 then 1 end;
select case 1 = 1 when 2 then 1 end;

-- Below queries check NULL usage in case statement (Second query fails in Postgres for type mismatch for second when clause)
select case NULL when NULL then 'abcd' when 'ab' then 'efgh' else 'xyz' end;
select case NULL when NULL then 'abcd' when 1 then 'efgh' else 'xyz' end;

-- Below queries check Cast between numeric and interger values in case statements (Work both in postgres and octo)
select case 1.5 when 1 then 'abcd' else 'efgh' end;
select case 1 when 1.5 then 'abcd' else 'efgh' end;

-- Below queries verify syntax highlighting of error queries happen for the entire query element which caused the error
select case when 12 then 'MyZero' end from names;
select case 'll' when 12 then 'MyZero' end from names;
select case when 12.33 then 'MyZero' end from names;
select case "asdf" when 12.33 then 'MyZero' end from names;
select case "asdf" when NULL then 'MyZero' end from names;
select case 112 when NULL then 'MyZero' end from names;
