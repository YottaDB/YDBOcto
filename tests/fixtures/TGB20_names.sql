#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Basic test
select lastname,id from names group by 1,2;
select lastname from names group by 1;
select * from names group by 1,2,3;
select n1.* from names n1 group by 1,2,3;
select 1 from names group by 1;
select 1+1 from names group by 1;
select firstname != 'Zero',count(firstname) from names group by firstname;

-- Test group by column number refers to select column list column rather than input column
select 'Zero' != 'Zero' as firstname from names group by 1;

-- Test group by column mentioned is taken as input column rather than output column
select 'Zero' != 'Zero' as firstname from names group by firstname;

-- Expressions test
select 'Cool'!=lastname from names group by 'Cool'!=lastname;
select firstname != 'Zero',count(firstname) from names group by firstname != 'Zero';
select firstname from names group by firstname!='Cool',firstname;
select 1 + 1 from names group by 1 + 0;
select 1 + 0 from names group by 1 + 0;
select id + 1 from names group by 1;
select firstname || 'Cool' from names group by 1;
select firstname from names group by firstname != 'Zero', 1;
select TRUE!=((lastname='Zero')!=TRUE) from names group by TRUE!=((lastname='Zero')!=TRUE);
-- Following two queries demonstrate that if at the least the column participating in an expression is present in
-- GroupBy the query will work.
select 'Cool'!=firstname from names group by firstname;
select TRUE!=((lastname='Cool')!=TRUE) from names group by TRUE!=((lastname='Zero')!=TRUE), lastname;
select ('Cool' = 'Zero') and (lastname = 'Cool'), count(lastname) from names group by lastname = 'Cool';
select ('Cool' = 'Zero') and (lastname = 'Cool'), count(lastname) from names group by lastname;
select (count(firstname) = 2) and (lastname = 'Cool'), count(lastname) from names group by lastname = 'Cool';
select (count(firstname) = 2) and (lastname = 'Cool'), count(lastname) from names group by lastname;
select 1 from (values(1),(2),(2))n1 group by (1+1);
select * from names group by id,firstname,1+id,lastname;
select firstname != lastname from names group by firstname != lastname;
select ('Cool' != firstname)=('Zero' != lastname) from names group by ('Cool' != firstname)=('Zero' != lastname);
select ('Cool' != firstname)=('Zero' != lastname) from names group by firstname,lastname;
select 1 from names group by id not in (1,2);
select id not in (1,2) from names group by id not in (1,2);
select firstname not in ('Cool','Zero') from names group by firstname not in ('Cool','Zero');
select firstname in ('Cool','Zero') from names group by firstname in ('Cool','Zero');

-- Following queries validate `is null`, `is not null` and `regex usage`
select lastname is null, count(firstname) from names group by lastname is null;
select lastname is null, count(firstname) from names group by lastname;
select count(firstname) from names group by lastname is null;
select firstname is not null,count(firstname) from names group by firstname;
select firstname is not null,count(firstname) from names group by firstname is not null;



-- But constants present in select column list can be refered by the group by using column number
-- This usage is allowed and works similar to postgres
select 'Zero',count(id) from names group by 1;
select 'Zero' from names group by 1;

-- Unary operations
select not firstname = 'Zero', count(firstname) from names group by firstname = 'Zero';
select not firstname = 'Zero', count(firstname) from names group by not firstname = 'Zero';
select not firstname = 'Zero', count(firstname) from names group by firstname = 'Zero', not firstname = 'Zero';
select 1 from names group by not 'Cool'='cool';
select +id from names group by +id;
select -id from names group by -id;
select +id from names group by id;

-- Following queries validate `is null`, `is not null` and `regex usage` in unary operations
select not lastname like 'Z%', count(firstname) from names group by not lastname like 'Z%';
select not lastname like 'Z%', count(firstname) from names group by lastname;
select not lastname is null from names group by lastname;
select not lastname is null from names group by lastname is null;
select not lastname is null from names group by not lastname is null;
select not lastname is not null from names group by lastname;
select not lastname is not null from names group by lastname is not null;
select not lastname is not null from names group by not lastname is not null;

-- Following queries validate that queries with multiple occurences of same node in GroupBy work fine
select lastname,id from names group by 1,1,2,2;
select lastname,id from names group by 1,2,1,1;
select 1+1 from names group by 1,1,1;
select firstname != 'Zero' from names group by firstname,firstname,firstname;
select firstname != 'Zero' from names group by firstname != 'Zero', firstname != 'Zero', firstname != 'Zero';
select firstname not in ('Cool','Zero') from names group by firstname not in ('Cool','Zero'), firstname not in ('Cool','Zero');

-- Following queries validate string concatenation expressions in GroupBy
select firstname || 'x' from names group by firstname || 'x';
select firstname || 'x' as firstname, lastname || 'x' as lastname from names group by firstname || 'x', lastname || 'x';
select firstname || 'x', lastname || 'x' as lastname from names group by firstname || 'x', lastname || 'x';
select firstname || 'x' as firstname, lastname || 'x' as lastname from names group by firstname || 'x', 2;
select firstname || 'x' as firstname, lastname || 'x' as lastname from names group by 1, 2;
select firstname || 'x' as firstname, lastname || 'x' as lastname from names group by 2,1;
select firstname || 'x' as firstname, lastname || 'x' as lastname from names group by firstname, lastname || 'x';

-- Different expression variations with Having
SELECT id+1 from names group by id having id=1;
SELECT id+1 from names group by id having (id+1)=1;
SELECT id+1 from names group by id having (id+1+1)=1;
SELECT id+1 from names group by id+1 having (id+1+1)=1;
select id+1 from names group by id having (id+1)=1 order by id+1;
select id+1 from names group by id+1 having (id+1)=1 order by id+1;
SELECT id,firstname FROM names n1 group by n1.id, n1.firstname having exists (SELECT n2.id FROM names n2 group by n1.id,n2.id having n2.id>1);

-- CASE statement based
select n1.id+1 from names n1 group by n1.id+1 having CASE n1.id+1 WHEN 2 THEN TRUE END;
select n1.id+1 from names n1 group by n1.id+1 having CASE n1.id+1 WHEN 2 THEN TRUE END;
select CASE n1.id+1 WHEN 2 THEN 1 END  from names n1 group by n1.id having CASE n1.id+1 WHEN 2 THEN TRUE END;
select CASE n1.id+1 WHEN 2 THEN 1 END  from names n1 group by n1.id+1 having CASE n1.id+1 WHEN 2 THEN TRUE END;
select count(n1.*) from names n1 group by n1.id+2 having CASE count(n1.id)+1 WHEN 2 THEN TRUE END;
select 1 from names n1 group by n1.id+2 having CASE 2 WHEN n1.id+2 THEN TRUE END;
select CASE n1.id+1 WHEN 2 THEN TRUE END from names n1 group by 1 having CASE n1.id+1 WHEN 2 THEN TRUE END;
select CASE n1.id+1 WHEN 2 THEN 1 END  from names n1 group by n1.id+1, CASE n1.id+1 WHEN 2 THEN 1 END having CASE n1.id+1 WHEN 2 THEN TRUE END;
select CASE n1.id+1 WHEN 2 THEN TRUE END  from names n1 group by CASE n1.id+1 WHEN 2 THEN TRUE END having CASE n1.id+1 WHEN 2 THEN TRUE END;

-- Misc
select count(n1.*) from names as n1 group by 1!=abs(n1.id);
select max(id) from names group by 1 + 0;
SELECT +((id - 4)) FROM names GROUP BY +((id - 4)) ORDER BY +((id - 4));
select id from names n2 where exists (select n1.firstname,n2.lastname from names n1 group by n1.firstname);
select id from names n2 where exists (select n1.firstname,n2.lastname||'test' from names n1 group by n1.firstname);
select id from names n2 where exists (select n1.firstname,n2.lastname||'test' from names n1 group by n1.firstname,n2.lastname||'test');

-- Extensive variations
-- literal_value
-- -- STRING_LITERAL
SELECT 'test' FROM names GROUP BY 1;
SELECT 'test' FROM names GROUP BY 1 HAVING 'test'!='hello';
SELECT 'test' FROM names GROUP BY firstname HAVING 'test'!='hello' ORDER BY 1;

-- -- NULL
SELECT NULL FROM names GROUP BY 1;
SELECT NULL FROM names GROUP BY 1 HAVING NULL!='hello';
SELECT NULL FROM names GROUP BY firstname HAVING NULL!='hello' ORDER BY 1;

-- TRUE
SELECT TRUE FROM names GROUP BY TRUE;
SELECT TRUE FROM names GROUP BY TRUE HAVING TRUE!=FALSE;
SELECT TRUE FROM names GROUP BY TRUE ORDER BY TRUE;
SELECT TRUE FROM names GROUP BY 1;
SELECT TRUE FROM names GROUP BY 1 ORDER BY TRUE;
SELECT TRUE FROM names GROUP BY 1 HAVING TRUE!=FALSE ORDER BY TRUE;
SELECT TRUE FROM names GROUP BY 1 HAVING TRUE!=FALSE;
SELECT TRUE FROM names GROUP BY firstname HAVING TRUE!=FALSE ORDER BY TRUE;
SELECT TRUE FROM names GROUP BY firstname HAVING TRUE!=FALSE ORDER BY 1;

-- Coalesce
SELECT coalesce(NULL,'test','hello') FROM names;
SELECT coalesce('soml','test','hello') FROM names;
SELECT coalesce('soml') FROM names;
SELECT coalesce(NULL,'lav','dre') FROM names GROUP BY coalesce(NULL,'lav','dre');
SELECT coalesce('soml','loms') FROM names GROUP BY coalesce(NULL,'lav','dre');
SELECT 1 FROM names GROUP BY coalesce(NULL,'lav','dre');
SELECT coalesce(NULL,'soml','loms') FROM names GROUP BY 1;
SELECT coalesce('soml','loms') FROM names GROUP BY coalesce(NULL,'lav','dre') HAVING 1=1;
SELECT coalesce('soml','loms') FROM names GROUP BY coalesce(NULL,'lav','dre') HAVING coalesce('soml','loms')='soml';
SELECT coalesce('soml','loms') FROM names GROUP BY coalesce(NULL,'lav','dre') HAVING coalesce(NULL,'lav','dre')=NULL;
SELECT coalesce('soml','loms') FROM names GROUP BY coalesce(NULL,'lav','dre') HAVING coalesce(NULL,'lav','dre') IS NOT NULL;
SELECT coalesce('soml','loms') FROM names GROUP BY coalesce(NULL,'lav','dre') HAVING 1=1 ORDER BY coalesce('soml','loms');
SELECT coalesce('soml','loms') FROM names GROUP BY coalesce(NULL,'lav','dre') HAVING 1=1 ORDER BY 1;
SELECT coalesce('soml','loms') FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT coalesce('soml','loms') FROM names GROUP BY 1 HAVING coalesce(NULL,'lav','dre') IS NOT NULL ORDER BY 1;

-- null_if
SELECT nullif(NULL,'test') FROM names;
SELECT 1 FROM names GROUP BY nullif('test','test');
SELECT nullif('soml','loms') FROM names GROUP BY nullif('test','test') HAVING 1=1;
SELECT nullif('soml','loms') FROM names GROUP BY nullif('test','test') HAVING nullif('soml','loms')='soml';
SELECT nullif('soml','loms') FROM names GROUP BY nullif('test','test') HAVING nullif('test','test')=NULL;
SELECT nullif('soml','loms') FROM names GROUP BY nullif('test','test') HAVING nullif('test','test') IS NOT NULL;
SELECT nullif('soml','loms') FROM names GROUP BY nullif('test','test') HAVING 1=1 ORDER BY nullif('soml','loms');
SELECT nullif('soml','loms') FROM names GROUP BY nullif('test','test') HAVING 1=1 ORDER BY 1;
SELECT nullif('soml','loms') FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT nullif('soml','loms') FROM names GROUP BY 1 HAVING nullif('test','test') IS NOT NULL ORDER BY 1;

-- greatest
SELECT greatest(NULL,'test') FROM names;
SELECT greatest('soml','test','hello') FROM names;
SELECT greatest('soml') FROM names;
SELECT greatest('test','test') FROM names GROUP BY greatest('test','test');
SELECT greatest('soml','loms','mosl') FROM names GROUP BY greatest('test','test');
SELECT 1 FROM names GROUP BY greatest('test','test');
-- Uncomment the following once #794 is fixed
-- SELECT greatest(NULL,'soml','loms') FROM names GROUP BY 1;
SELECT greatest('soml','loms','mosl') FROM names GROUP BY greatest('test','test') HAVING 1=1;
SELECT greatest('soml','loms','mosl') FROM names GROUP BY greatest('test','test') HAVING greatest('soml','loms','mosl')='soml';
SELECT greatest('soml','loms','mosl') FROM names GROUP BY greatest('test','test') HAVING greatest('test','test')=NULL;
SELECT greatest('soml','loms','mosl') FROM names GROUP BY greatest('test','test') HAVING greatest('test','test') IS NOT NULL;
SELECT greatest('soml','loms','mosl') FROM names GROUP BY greatest('test','test') HAVING 1=1 ORDER BY greatest('soml','loms','mosl');
SELECT greatest('soml','loms','mosl') FROM names GROUP BY greatest('test','test') HAVING 1=1 ORDER BY 1;
SELECT greatest('soml','loms','mosl') FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT greatest('soml','loms','mosl') FROM names GROUP BY 1 HAVING greatest('test','test') IS NOT NULL ORDER BY 1;

-- least
SELECT least(NULL,'test') FROM names;
SELECT least('soml','test','hello') FROM names;
SELECT least('soml') FROM names;
SELECT least('test','test') FROM names GROUP BY least('test','test');
SELECT least('soml','loms','mosl') FROM names GROUP BY least('test','test');
SELECT 1 FROM names GROUP BY least('test','test');
-- Uncomment the following once #794 is fixed
-- SELECT least(NULL,'soml','loms') FROM names GROUP BY 1;
SELECT least('soml','loms','mosl') FROM names GROUP BY least('test','test') HAVING 1=1;
SELECT least('soml','loms','mosl') FROM names GROUP BY least('test','test') HAVING least('soml','loms','mosl')='soml';
SELECT least('soml','loms','mosl') FROM names GROUP BY least('test','test') HAVING least('test','test')=NULL;
SELECT least('soml','loms','mosl') FROM names GROUP BY least('test','test') HAVING least('test','test') IS NOT NULL;
SELECT least('soml','loms','mosl') FROM names GROUP BY least('test','test') HAVING 1=1 ORDER BY least('soml','loms','mosl');
SELECT least('soml','loms','mosl') FROM names GROUP BY least('test','test') HAVING 1=1 ORDER BY 1;
SELECT least('soml','loms','mosl') FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT least('soml','loms','mosl') FROM names GROUP BY 1 HAVING least('test','test') IS NOT NULL ORDER BY 1;

-- INTEGER_LITERAL
SELECT 12 FROM names GROUP BY 1;
SELECT 12 FROM names GROUP BY 1 HAVING 12=12;
SELECT 12 FROM names GROUP BY 1 HAVING 12=12 ORDER BY 1;
SELECT 12 FROM names GROUP BY firstname HAVING 12=12 ORDER BY 1;

SELECT coalesce(NULL,2,3) FROM names;
SELECT coalesce(1,2,4) FROM names;
SELECT coalesce(1) FROM names;
SELECT coalesce(NULL,2,3) FROM names GROUP BY coalesce(NULL,2,3);
SELECT coalesce(5,4) FROM names GROUP BY coalesce(NULL,2,3);
SELECT 1 FROM names GROUP BY coalesce(NULL,2,3);
SELECT coalesce(NULL,5,4) FROM names GROUP BY 1;
SELECT coalesce(5,4) FROM names GROUP BY coalesce(NULL,2,3) HAVING 1=1;
SELECT coalesce(5,4) FROM names GROUP BY coalesce(NULL,2,3) HAVING coalesce(5,4)=5;
SELECT coalesce(5,4) FROM names GROUP BY coalesce(NULL,2,3) HAVING coalesce(NULL,2,3)=NULL;
SELECT coalesce(5,4) FROM names GROUP BY coalesce(NULL,2,3) HAVING coalesce(NULL,2,3) IS NOT NULL;
SELECT coalesce(5,4) FROM names GROUP BY coalesce(NULL,2,3) HAVING 1=1 ORDER BY coalesce(5,4);
SELECT coalesce(5,4) FROM names GROUP BY coalesce(NULL,2,3) HAVING 1=1 ORDER BY 1;
SELECT coalesce(5,4) FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT coalesce(5,4) FROM names GROUP BY 1 HAVING coalesce(NULL,2,3) IS NOT NULL ORDER BY 1;

SELECT nullif(NULL,2) FROM names;
SELECT nullif(1,2) FROM names;
SELECT nullif(NULL,2) FROM names GROUP BY nullif(NULL,2);
SELECT nullif(2,2) FROM names GROUP BY nullif(NULL,2);
SELECT 1 FROM names GROUP BY nullif(NULL,2);
SELECT nullif(2,2) FROM names GROUP BY nullif(NULL,2) HAVING 1=1;
SELECT nullif(2,2) FROM names GROUP BY nullif(NULL,2) HAVING nullif(2,2)=5;
SELECT nullif(2,2) FROM names GROUP BY nullif(NULL,2) HAVING nullif(NULL,2)=NULL;
SELECT nullif(2,2) FROM names GROUP BY nullif(NULL,2) HAVING nullif(NULL,2) IS NOT NULL;
SELECT nullif(2,2) FROM names GROUP BY nullif(NULL,2) HAVING 1=1 ORDER BY nullif(2,2);
SELECT nullif(2,2) FROM names GROUP BY nullif(NULL,2) HAVING 1=1 ORDER BY 1;
SELECT nullif(2,2) FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT nullif(2,2) FROM names GROUP BY 1 HAVING nullif(NULL,2) IS NOT NULL ORDER BY 1;

SELECT greatest(NULL,2) FROM names;
SELECT greatest(1,2) FROM names;
SELECT greatest(1) FROM names;
SELECT greatest(NULL,2) FROM names GROUP BY greatest(NULL,2);
SELECT greatest(3,1,2) FROM names GROUP BY greatest(NULL,2);
SELECT 1 FROM names GROUP BY greatest(NULL,2);
SELECT greatest(NULL,5,4) FROM names GROUP BY 1;
SELECT greatest(3,1,2) FROM names GROUP BY greatest(NULL,2) HAVING 1=1;
SELECT greatest(3,1,2) FROM names GROUP BY greatest(NULL,2) HAVING greatest(3,1,2)=5;
SELECT greatest(3,1,2) FROM names GROUP BY greatest(NULL,2) HAVING greatest(NULL,2)=NULL;
SELECT greatest(3,1,2) FROM names GROUP BY greatest(NULL,2) HAVING greatest(NULL,2) IS NOT NULL;
SELECT greatest(3,1,2) FROM names GROUP BY greatest(NULL,2) HAVING 1=1 ORDER BY greatest(3,1,2);
SELECT greatest(3,1,2) FROM names GROUP BY greatest(NULL,2) HAVING 1=1 ORDER BY 1;
SELECT greatest(3,1,2) FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT greatest(3,1,2) FROM names GROUP BY 1 HAVING greatest(NULL,2) IS NOT NULL ORDER BY 1;

SELECT least(NULL,2) FROM names;
SELECT least(1,2) FROM names;
SELECT least(1) FROM names;
SELECT least(NULL,2) FROM names GROUP BY least(NULL,2);
SELECT least(3,1,2) FROM names GROUP BY least(NULL,2);
SELECT 1 FROM names GROUP BY least(NULL,2);
SELECT least(NULL,5,4) FROM names GROUP BY 1;
SELECT least(3,1,2) FROM names GROUP BY least(NULL,2) HAVING 1=1;
SELECT least(3,1,2) FROM names GROUP BY least(NULL,2) HAVING least(3,1,2)=5;
SELECT least(3,1,2) FROM names GROUP BY least(NULL,2) HAVING least(NULL,2)=NULL;
SELECT least(3,1,2) FROM names GROUP BY least(NULL,2) HAVING least(NULL,2) IS NOT NULL;
SELECT least(3,1,2) FROM names GROUP BY least(NULL,2) HAVING 1=1 ORDER BY least(3,1,2);
SELECT least(3,1,2) FROM names GROUP BY least(NULL,2) HAVING 1=1 ORDER BY 1;
SELECT least(3,1,2) FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT least(3,1,2) FROM names GROUP BY 1 HAVING least(NULL,2) IS NOT NULL ORDER BY 1;

-- literal_value
-- -- NUMERIC_LITERAL
SELECT 1.2 FROM names GROUP BY 1;
SELECT 1.2 FROM names GROUP BY 1 HAVING 1.2=1.2;
SELECT 1.2 FROM names GROUP BY 1 HAVING 1.2=1.2 ORDER BY 1;
SELECT 1.2 FROM names GROUP BY firstname HAVING 1.2=1.2 ORDER BY 1;

SELECT coalesce(NULL,5.5,4.3) FROM names GROUP BY 1;
SELECT coalesce(5.5,4.3) FROM names GROUP BY coalesce(NULL,2.3,3) HAVING 1=1;
SELECT coalesce(5.5,4.3) FROM names GROUP BY coalesce(NULL,2.3,3) HAVING coalesce(5.5,4.3)=5.5;
SELECT coalesce(5.5,4.3) FROM names GROUP BY coalesce(NULL,2.3,3) HAVING coalesce(NULL,2.3,3)=NULL;
SELECT coalesce(5.5,4.3) FROM names GROUP BY coalesce(NULL,2.3,3) HAVING coalesce(NULL,2.3,3) IS NOT NULL;
SELECT coalesce(5.5,4.3) FROM names GROUP BY coalesce(NULL,2.3,3) HAVING 1=1 ORDER BY coalesce(5.5,4.3);
SELECT coalesce(5.5,4.3) FROM names GROUP BY coalesce(NULL,2.3,3) HAVING 1=1 ORDER BY 1;
SELECT coalesce(5.5,4.3) FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT coalesce(5.5,4.3) FROM names GROUP BY 1 HAVING coalesce(NULL,2.3,3) IS NOT NULL ORDER BY 1;

SELECT nullif(NULL,5.5) FROM names GROUP BY 1;
SELECT nullif(5.5,4.3) FROM names GROUP BY nullif(NULL,2.3) HAVING 1=1;
SELECT nullif(5.5,4.3) FROM names GROUP BY nullif(NULL,2.3) HAVING nullif(5.5,4.3)=5.5;
SELECT nullif(5.5,4.3) FROM names GROUP BY nullif(NULL,2.3) HAVING nullif(NULL,2.3)=NULL;
SELECT nullif(5.5,4.3) FROM names GROUP BY nullif(NULL,2.3) HAVING nullif(NULL,2.3) IS NOT NULL;
SELECT nullif(5.5,4.3) FROM names GROUP BY nullif(NULL,2.3) HAVING 1=1 ORDER BY nullif(5.5,4.3);
SELECT nullif(5.5,4.3) FROM names GROUP BY nullif(NULL,2.3) HAVING 1=1 ORDER BY 1;
SELECT nullif(5.5,4.3) FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT nullif(5.5,4.3) FROM names GROUP BY 1 HAVING nullif(NULL,2.3) IS NOT NULL ORDER BY 1;
SELECT nullif(5.5,4.3) FROM names GROUP BY nullif(5,4);
SELECT nullif(5.5,4.3) FROM names GROUP BY nullif(5,4) ORDER BY 1;

SELECT greatest(NULL,5.5) FROM names GROUP BY 1;
SELECT greatest(5.5,4.3) FROM names GROUP BY greatest(NULL,2.3) HAVING 1=1;
SELECT greatest(5.5,4.3) FROM names GROUP BY greatest(NULL,2.3) HAVING greatest(5.5,4.3)=5.5;
SELECT greatest(5.5,4.3) FROM names GROUP BY greatest(NULL,2.3) HAVING greatest(NULL,2.3)=NULL;
SELECT greatest(5.5,4.3) FROM names GROUP BY greatest(NULL,2.3) HAVING greatest(NULL,2.3) IS NOT NULL;
SELECT greatest(5.5,4.3) FROM names GROUP BY greatest(NULL,2.3) HAVING 1=1 ORDER BY greatest(5.5,4.3);
SELECT greatest(5.5,4.3) FROM names GROUP BY greatest(NULL,2.3) HAVING 1=1 ORDER BY 1;
SELECT greatest(5.5,4.3) FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT greatest(5.5,4.3) FROM names GROUP BY 1 HAVING greatest(NULL,2.3) IS NOT NULL ORDER BY 1;
SELECT greatest(5.5,4.3) FROM names GROUP BY greatest(5,4);
SELECT greatest(5.5,4.3) FROM names GROUP BY greatest(5,4) ORDER BY 1;

SELECT least(NULL,5.5) FROM names GROUP BY 1;
SELECT least(5.5,4.3) FROM names GROUP BY least(NULL,2.3) HAVING 1=1;
SELECT least(5.5,4.3) FROM names GROUP BY least(NULL,2.3) HAVING least(5.5,4.3)=5.5;
SELECT least(5.5,4.3) FROM names GROUP BY least(NULL,2.3) HAVING least(NULL,2.3)=NULL;
SELECT least(5.5,4.3) FROM names GROUP BY least(NULL,2.3) HAVING least(NULL,2.3) IS NOT NULL;
SELECT least(5.5,4.3) FROM names GROUP BY least(NULL,2.3) HAVING 1=1 ORDER BY least(5.5,4.3);
SELECT least(5.5,4.3) FROM names GROUP BY least(NULL,2.3) HAVING 1=1 ORDER BY 1;
SELECT least(5.5,4.3) FROM names GROUP BY 1 HAVING 1=1 ORDER BY 1;
SELECT least(5.5,4.3) FROM names GROUP BY 1 HAVING least(NULL,2.3) IS NOT NULL ORDER BY 1;
SELECT least(5.5,4.3) FROM names GROUP BY least(5,4);
SELECT least(5.5,4.3) FROM names GROUP BY least(5,4) ORDER BY 1;

-- column_reference
-- -- IDENTIFIER_ALONE (column)
SELECT firstname FROM names GROUP BY firstname;
SELECT firstname FROM names GROUP BY firstname HAVING firstname!='COOL';
SELECT firstname FROM names GROUP BY firstname HAVING firstname!='COOL' ORDER BY firstname;
SELECT firstname FROM names GROUP BY 1;
SELECT firstname FROM names GROUP BY 1 HAVING firstname!='Cool';
SELECT firstname FROM names GROUP BY 1 ORDER BY firstname;
SELECT firstname FROM names GROUP BY 1 HAVING firstname!='Cool' ORDER BY firstname;
SELECT firstname FROM names GROUP BY 1 HAVING firstname!='Cool' ORDER BY 1;

SELECT coalesce(lastname) FROM names GROUP BY coalesce(lastname);
SELECT coalesce(lastname) FROM names GROUP BY coalesce(lastname) HAVING coalesce(lastname)!='COOL';
SELECT coalesce(lastname) FROM names GROUP BY coalesce(lastname) HAVING coalesce(lastname)!='COOL' ORDER BY coalesce(lastname);
SELECT coalesce(lastname) FROM names GROUP BY 1;
SELECT coalesce(lastname) FROM names GROUP BY 1 HAVING coalesce(lastname)!='Cool';
SELECT coalesce(lastname) FROM names GROUP BY 1 ORDER BY coalesce(lastname);
SELECT coalesce(lastname) FROM names GROUP BY 1 HAVING coalesce(lastname)!='COOL' ORDER BY coalesce(lastname);
SELECT coalesce(lastname) FROM names GROUP BY 1 HAVING coalesce(lastname)!='COOL' ORDER BY 1;

SELECT coalesce(lastname,firstname) FROM names GROUP BY coalesce(lastname,firstname);
SELECT coalesce(lastname,firstname) FROM names GROUP BY coalesce(lastname,firstname) HAVING coalesce(lastname,firstname)!='COOL';
SELECT coalesce(lastname,firstname) FROM names GROUP BY coalesce(lastname,firstname) HAVING coalesce(lastname,firstname)!='COOL' ORDER BY coalesce(lastname,firstname);
SELECT coalesce(lastname,firstname) FROM names GROUP BY 1;
SELECT coalesce(lastname,firstname) FROM names GROUP BY 1 HAVING coalesce(lastname,firstname)!='Cool';
SELECT coalesce(lastname,firstname) FROM names GROUP BY 1 ORDER BY coalesce(lastname,firstname);
SELECT coalesce(lastname,firstname) FROM names GROUP BY 1 HAVING coalesce(lastname,firstname)!='COOL' ORDER BY coalesce(lastname,firstname);
SELECT coalesce(lastname,firstname) FROM names GROUP BY 1 HAVING coalesce(lastname,firstname)!='COOL' ORDER BY 1;
SELECT coalesce(lastname,firstname) FROM names GROUP BY lastname,firstname;
SELECT 1 FROM names GROUP BY COALESCE(lastname,firstname) ORDER BY coalesce(lastname,firstname);
SELECT 1 FROM names GROUP BY COALESCE(lastname,firstname) HAVING coalesce(lastname,firstname)!='cool';
SELECT 1 FROM names GROUP BY COALESCE(lastname,firstname) HAVING coalesce(lastname,firstname)!='cool' ORDER BY 1;
SELECT coalesce(lastname,firstname) FROM names GROUP BY lastname,firstname,coalesce(lastname,'test');
SELECT coalesce(lastname,firstname) FROM names GROUP BY lastname,firstname,coalesce(lastname,'test') ORDER BY 1;
SELECT coalesce(lastname,firstname) FROM names GROUP BY lastname,firstname,coalesce(lastname,'test') HAVING lastname != 'Cool' ORDER BY 1;

SELECT nullif(lastname,firstname) FROM names GROUP BY nullif(lastname,firstname);
SELECT nullif(lastname,firstname) FROM names GROUP BY nullif(lastname,firstname) HAVING nullif(lastname,firstname)!='COOL';
SELECT nullif(lastname,firstname) FROM names GROUP BY nullif(lastname,firstname) HAVING nullif(lastname,firstname)!='COOL' ORDER BY nullif(lastname,firstname);
SELECT nullif(lastname,firstname) FROM names GROUP BY 1;
SELECT nullif(lastname,firstname) FROM names GROUP BY 1 HAVING nullif(lastname,firstname)!='Cool';
SELECT nullif(lastname,firstname) FROM names GROUP BY 1 ORDER BY nullif(lastname,firstname);
SELECT nullif(lastname,firstname) FROM names GROUP BY 1 HAVING nullif(lastname,firstname)!='COOL' ORDER BY nullif(lastname,firstname);
SELECT nullif(lastname,firstname) FROM names GROUP BY 1 HAVING nullif(lastname,firstname)!='COOL' ORDER BY 1;
SELECT nullif(lastname,firstname) FROM names GROUP BY lastname,firstname;
SELECT 1 FROM names GROUP BY nullif(lastname,firstname) ORDER BY nullif(lastname,firstname);
SELECT 1 FROM names GROUP BY nullif(lastname,firstname) HAVING nullif(lastname,firstname)!='cool';
SELECT 1 FROM names GROUP BY nullif(lastname,firstname) HAVING nullif(lastname,firstname)!='cool' ORDER BY 1;
SELECT nullif(lastname,firstname) FROM names GROUP BY lastname,firstname,nullif(lastname,'test');
SELECT nullif(lastname,firstname) FROM names GROUP BY lastname,firstname,nullif(lastname,'test') ORDER BY 1;
SELECT nullif(lastname,firstname) FROM names GROUP BY lastname,firstname,nullif(lastname,'test') HAVING lastname != 'Cool' ORDER BY 1;

SELECT greatest(lastname,firstname) FROM names GROUP BY greatest(lastname,firstname);
SELECT greatest(lastname,firstname) FROM names GROUP BY greatest(lastname,firstname) HAVING greatest(lastname,firstname)!='COOL';
SELECT greatest(lastname,firstname) FROM names GROUP BY greatest(lastname,firstname) HAVING greatest(lastname,firstname)!='COOL' ORDER BY greatest(lastname,firstname);
SELECT greatest(lastname,firstname) FROM names GROUP BY 1;
SELECT greatest(lastname,firstname) FROM names GROUP BY 1 HAVING greatest(lastname,firstname)!='Cool';
SELECT greatest(lastname,firstname) FROM names GROUP BY 1 ORDER BY greatest(lastname,firstname);
SELECT greatest(lastname,firstname) FROM names GROUP BY 1 HAVING greatest(lastname,firstname)!='COOL' ORDER BY greatest(lastname,firstname);
SELECT greatest(lastname,firstname) FROM names GROUP BY 1 HAVING greatest(lastname,firstname)!='COOL' ORDER BY 1;
SELECT greatest(lastname,firstname) FROM names GROUP BY lastname,firstname;
SELECT 1 FROM names GROUP BY greatest(lastname,firstname) ORDER BY greatest(lastname,firstname);
SELECT 1 FROM names GROUP BY greatest(lastname,firstname) HAVING greatest(lastname,firstname)!='cool';
SELECT 1 FROM names GROUP BY greatest(lastname,firstname) HAVING greatest(lastname,firstname)!='cool' ORDER BY 1;
SELECT greatest(lastname,firstname) FROM names GROUP BY lastname,firstname,greatest(lastname,'test');
SELECT greatest(lastname,firstname) FROM names GROUP BY lastname,firstname,greatest(lastname,'test') ORDER BY 1;
SELECT greatest(lastname,firstname) FROM names GROUP BY lastname,firstname,greatest(lastname,'test') HAVING lastname != 'Cool' ORDER BY 1;

SELECT least(lastname,firstname) FROM names GROUP BY least(lastname,firstname);
SELECT least(lastname,firstname) FROM names GROUP BY least(lastname,firstname) HAVING least(lastname,firstname)!='COOL';
SELECT least(lastname,firstname) FROM names GROUP BY least(lastname,firstname) HAVING least(lastname,firstname)!='COOL' ORDER BY least(lastname,firstname);
SELECT least(lastname,firstname) FROM names GROUP BY 1;
SELECT least(lastname,firstname) FROM names GROUP BY 1 HAVING least(lastname,firstname)!='Cool';
SELECT least(lastname,firstname) FROM names GROUP BY 1 ORDER BY least(lastname,firstname);
SELECT least(lastname,firstname) FROM names GROUP BY 1 HAVING least(lastname,firstname)!='COOL' ORDER BY least(lastname,firstname);
SELECT least(lastname,firstname) FROM names GROUP BY 1 HAVING least(lastname,firstname)!='COOL' ORDER BY 1;
SELECT least(lastname,firstname) FROM names GROUP BY lastname,firstname;
SELECT 1 FROM names GROUP BY least(lastname,firstname) ORDER BY least(lastname,firstname);
SELECT 1 FROM names GROUP BY least(lastname,firstname) HAVING least(lastname,firstname)!='cool';
SELECT 1 FROM names GROUP BY least(lastname,firstname) HAVING least(lastname,firstname)!='cool' ORDER BY 1;
SELECT least(lastname,firstname) FROM names GROUP BY lastname,firstname,least(lastname,'test');
SELECT least(lastname,firstname) FROM names GROUP BY lastname,firstname,least(lastname,'test') ORDER BY 1;
SELECT least(lastname,firstname) FROM names GROUP BY lastname,firstname,least(lastname,'test') HAVING lastname != 'Cool' ORDER BY 1;

-- column_reference
-- -- IDENTIFIER_PERIOD_IDENTIFIER (table.column)
SELECT n1.firstname FROM names n1 GROUP BY n1.firstname;
SELECT n1.firstname FROM names n1 GROUP BY n1.firstname HAVING n1.firstname!='COOL';
SELECT n1.firstname FROM names n1 GROUP BY n1.firstname HAVING n1.firstname!='COOL' ORDER BY n1.firstname;
SELECT n1.firstname FROM names n1 GROUP BY 1;
SELECT n1.firstname FROM names n1 GROUP BY 1 HAVING n1.firstname!='Cool';
SELECT n1.firstname FROM names n1 GROUP BY 1 ORDER BY n1.firstname;
SELECT n1.firstname FROM names n1 GROUP BY 1 HAVING n1.firstname!='Cool' ORDER BY n1.firstname;
SELECT n1.firstname FROM names n1 GROUP BY 1 HAVING n1.firstname!='Cool' ORDER BY 1;

-- cas_STATEMENT
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names;
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names GROUP BY CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end;
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names GROUP BY CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end HAVING 'four'!=CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end;
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names GROUP BY CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end HAVING 'four'=CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end;
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names GROUP BY CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end HAVING 'four'=CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end ORDER BY  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end;
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names GROUP BY CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end ORDER BY CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end;
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names GROUP BY CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end ORDER BY 1;
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names GROUP BY 1;
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names GROUP BY 1 ORDER BY 1;
SELECT  CASE 'test' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end FROM names GROUP BY 1 HAVING 'four'=CASE 'lleho' WHEN 'te'||'st' THEN 'one' WHEN 'hello' THEN 'three' ELSE 'four' end ORDER BY 1;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names;
SELECT CASE WHEN NULL THEN 'one' ELSE 'ninenine' end FROM names;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names GROUP BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names GROUP BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end HAVING 'second'!=CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names GROUP BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end HAVING 'second'=CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names GROUP BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end HAVING 'second'=CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end ORDER BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names GROUP BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end ORDER BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end HAVING 'second'!=CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end HAVING 'second'=CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end ORDER BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end ORDER BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names GROUP BY CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end ORDER BY 1;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names GROUP BY 1;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names GROUP BY 1 ORDER BY 1;
SELECT CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end FROM names GROUP BY 1 HAVING 'second'=CASE WHEN NULL THEN 'one' WHEN 'te'||'st'='test' THEN 'second' ELSE 'ninenine' end ORDER BY 1;

SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end HAVING 4!=CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end HAVING 4=CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end HAVING 4=CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end ORDER BY CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end HAVING 4=CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end ORDER BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 end;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end ORDER BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 end;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end ORDER BY 1;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY 1;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY 1 ORDER BY 1;
SELECT CASE 4 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end FROM names GROUP BY 1 HAVING 4=CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 ELSE 4 end ORDER BY 1;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names;
SELECT CASE WHEN NULL THEN 1 ELSE 99 end FROM names;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end HAVING 2!=CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end HAVING 2=CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end HAVING 2=CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end ORDER BY CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end HAVING 3=CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end ORDER BY CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end ORDER BY CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end HAVING 2!=CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end HAVING 2=CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end HAVING 2=CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end ORDER BY CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end ORDER BY CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end ORDER BY 1;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY 1;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY 1 ORDER BY 1;
SELECT CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end FROM names GROUP BY 1 HAVING 2=CASE WHEN NULL THEN 1 WHEN 1+1=2 THEN 2 ELSE 99 end ORDER BY 1;

SELECT CASE WHEN NULL THEN 1 end FROM names;
SELECT CASE WHEN NULL THEN 1 end FROM names GROUP BY CASE WHEN NULL THEN 1 end;
SELECT CASE WHEN NULL THEN 1 end FROM names GROUP BY CASE WHEN NULL THEN 1 end ORDER BY CASE WHEN NULL THEN 1 end;
SELECT CASE WHEN NULL THEN 1 end FROM names GROUP BY CASE WHEN NULL THEN 1 end HAVING NULL=CASE WHEN NULL THEN 1 end ORDER BY CASE WHEN NULL THEN 1 end;
SELECT CASE WHEN NULL THEN 1 end FROM names GROUP BY 1;
SELECT CASE WHEN NULL THEN 1 end FROM names GROUP BY 1 ORDER BY 1;
SELECT CASE WHEN NULL THEN 1 end FROM names GROUP BY 1 HAVING NULL=CASE WHEN NULL THEN 1 end ORDER BY 1;

SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end HAVING 9.9!=CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end HAVING 9.9=CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end HAVING 9.9=CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end ORDER BY  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end HAVING 9.9=CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end ORDER BY CASE 5.0 WHEN 2.0+2 THEN 1.0 WHEN 3.0 THEN 3.0 end;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end ORDER BY CASE 5 WHEN 2+2 THEN 1 WHEN 3 THEN 3 end;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end ORDER BY 1;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY 1;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY 1 ORDER BY 1;
SELECT  CASE 1.1 WHEN 1+0.1 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end FROM names GROUP BY 1 HAVING 9.9=CASE 5.0 WHEN 3.1+2 THEN 1.1 WHEN 2.1 THEN 1.2 ELSE 9.9 end ORDER BY 1;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names;
SELECT CASE WHEN NULL THEN 1 ELSE 99 end FROM names;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end HAVING 1.2!=CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end HAVING 1.2=CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end HAVING 1.2=CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end ORDER BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end ORDER BY CASE WHEN NULL THEN 1 WHEN 3+1=4 THEN 3 ELSE 99 end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end HAVING 2!=CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end HAVING 2=CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end HAVING 2=CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end ORDER BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end ORDER BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names GROUP BY CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end ORDER BY 1;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names GROUP BY 1;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names GROUP BY 1 ORDER BY 1;
SELECT CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end FROM names GROUP BY 1 HAVING 1.2=CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end ORDER BY 1;

SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names;
SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY 1;
SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY 1 ORDER BY 1;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names;
SELECT CASE WHEN NULL THEN firstname ELSE firstname||'test' end FROM names;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end HAVING 'Cool'!=CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end HAVING 'Cool'=CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end HAVING 'Cool'=CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end ORDER BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end ORDER BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end HAVING 'Cool'!=CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end HAVING 'Cool'=CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end HAVING 'Cool'=CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end ORDER BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end;
SELECT 1 FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end ORDER BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end ORDER BY 1;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY 1;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY 1 ORDER BY 1;
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY 1 HAVING 'Cool'=CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end ORDER BY 1;

select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names;
select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY firstname;
select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END;
select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY lastname,firstname;
select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY CASE lastname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END,firstname;
select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY 1,lastname ORDER BY CASE lastname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END; -- sort-needed-check
select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY lastname,1 HAVING 3=CASE lastname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END ORDER BY 1;

select CASE 'Cool' WHEN 'Cool' THEN firstname WHEN 'Zero' THEN 'test' ELSE 'default' END FROM names GROUP BY firstname;
select CASE 'Cool' WHEN 'Cool' THEN 'tset' WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY firstname;
select CASE 'Cool' WHEN 'Cool' THEN 'tset' WHEN lastname THEN 'test' ELSE firstname END FROM names GROUP BY lastname,firstname;

select CASE 'Cool' WHEN 'Cool' THEN lastname WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname,firstname;
select CASE 'Cool' WHEN 'Cool' THEN lastname WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname,firstname;
select CASE 'Cool' WHEN 'Cool' THEN lastname WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname,firstname ORDER BY lastname||'test';
select CASE 'Cool' WHEN 'Cool' THEN lastname||'test' WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname,firstname ORDER BY lastname||'test';
select CASE 'Cool' WHEN 'Cool' THEN lastname||'test' WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname||'test',firstname ORDER BY lastname||'test';
select CASE 'Cool' WHEN 'Cool' THEN lastname||'test' WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname||'test',firstname ORDER BY lastname||'test',firstname;
select CASE 'Cool' WHEN 'Cool' THEN lastname||'test' WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname||'test',firstname HAVING 'Burntest'=lastname||'test' ORDER BY lastname||'test',firstname;

-- aggregate function
select 1 from names group by 1 having 1!=count(DISTINCT lastname);
select 1 from names group by 1 having 1!=count(DISTINCT lastname) order by count(DISTINCT lastname);
select 1 from names group by 1 order by count(DISTINCT lastname);
select 1 from names group by firstname having 1!=count(DISTINCT lastname);
select 1 from names group by firstname having 1!=count(DISTINCT lastname) order by count(DISTINCT lastname);
select 1 from names group by firstname order by count(DISTINCT lastname);
select count(DISTINCT lastname) from names group by firstname having 0!=count(DISTINCT lastname) order by count(DISTINCT lastname);
select count(DISTINCT lastname) from names group by firstname having 0!=count(DISTINCT lastname) order by 1;
select 1+count(DISTINCT lastname) from names group by firstname order by 1+count(DISTINCT lastname);
select 1+count(DISTINCT lastname) from names group by firstname having 2=1+count(DISTINCT lastname)order by 1+count(DISTINCT lastname);

select count(DISTINCT 'hello'||lastname) from names;
select count(DISTINCT 'hello'||lastname) from names;
select count(DISTINCT 'hello'||lastname) from names group by 'hello'||lastname order by 'hello'||lastname;
select count(DISTINCT 'hello'||lastname) from names group by 'hello'||lastname order by 1;
select count(DISTINCT 'hello'||lastname) from names group by 'hello'||lastname;
select count(DISTINCT 'hello'||lastname) from names group by lastname;


select MAX(DISTINCT lastname) from names;
select 1 from names group by 1 having 'Burn'!=MAX(DISTINCT lastname);
select 1 from names group by 1 having 'Burn'!=MAX(DISTINCT lastname) order by MAX(DISTINCT lastname);
select 1 from names group by 1 order by MAX(DISTINCT lastname);
select 1 from names group by firstname having 'Burn'!=MAX(DISTINCT lastname);
select 1 from names group by firstname having 'Burn'!=MAX(DISTINCT lastname) order by MAX(DISTINCT lastname);
select 1 from names group by firstname order by MAX(DISTINCT lastname);
select MAX(DISTINCT lastname) from names group by firstname having 'Killer'!=MAX(DISTINCT lastname) order by MAX(DISTINCT lastname);
select MAX(DISTINCT lastname) from names group by firstname having 'Killer'!=MAX(DISTINCT lastname) order by 1;
select 'test'||MAX(DISTINCT lastname) from names group by firstname order by 'test'||MAX(DISTINCT lastname);
select 'test'||MAX(DISTINCT lastname) from names group by firstname having 'Cooltest'='test'||MAX(DISTINCT lastname)order by 'test'||MAX(DISTINCT lastname);

select MAX(DISTINCT 'hello'||lastname) from names;
select MAX(DISTINCT 'hello'||lastname) from names;
select MAX(DISTINCT 'hello'||lastname) from names group by 'hello'||lastname order by 'hello'||lastname;
select MAX(DISTINCT 'hello'||lastname) from names group by 'hello'||lastname order by 1;
select MAX(DISTINCT 'hello'||lastname) from names group by 'hello'||lastname;
select MAX(DISTINCT 'hello'||lastname) from names group by lastname;

select MIN(DISTINCT lastname) from names;
select 1 from names group by 1 having 'Burn'!=MIN(DISTINCT lastname);
select 1 from names group by 1 having 'Burn'!=MIN(DISTINCT lastname) order by MIN(DISTINCT lastname);
select 1 from names group by 1 order by MIN(DISTINCT lastname);
select 1 from names group by firstname having 'Burn'!=MIN(DISTINCT lastname);
select 1 from names group by firstname having 'Burn'!=MIN(DISTINCT lastname) order by MIN(DISTINCT lastname);
select 1 from names group by firstname order by MIN(DISTINCT lastname);
select MIN(DISTINCT lastname) from names group by firstname having 'Killer'!=MIN(DISTINCT lastname) order by MIN(DISTINCT lastname);
select MIN(DISTINCT lastname) from names group by firstname having 'Killer'!=MIN(DISTINCT lastname) order by 1;
select 'test'||MIN(DISTINCT lastname) from names group by firstname order by 'test'||MIN(DISTINCT lastname);
select 'test'||MIN(DISTINCT lastname) from names group by firstname having 'Cooltest'='test'||MIN(DISTINCT lastname)order by 'test'||MIN(DISTINCT lastname);

select MIN(DISTINCT 'hello'||lastname) from names;
select MIN(DISTINCT 'hello'||lastname) from names;
select MIN(DISTINCT 'hello'||lastname) from names group by 'hello'||lastname order by 'hello'||lastname;
select MIN(DISTINCT 'hello'||lastname) from names group by 'hello'||lastname order by 1;
select MIN(DISTINCT 'hello'||lastname) from names group by 'hello'||lastname;
select MIN(DISTINCT 'hello'||lastname) from names group by lastname;


select SUM(DISTINCT id) from names;
select 1 from names group by 1 having 1!=SUM(DISTINCT id);
select 1 from names group by 1 having 1!=SUM(DISTINCT id) order by SUM(DISTINCT id);
select 1 from names group by 1 order by SUM(DISTINCT id);
select 1 from names group by firstname having 1!=SUM(DISTINCT id);
select 1 from names group by firstname having 1!=SUM(DISTINCT id) order by SUM(DISTINCT id);
select 1 from names group by firstname order by SUM(DISTINCT id);
select SUM(DISTINCT id) from names group by firstname having 2!=SUM(DISTINCT id) order by SUM(DISTINCT id);
select SUM(DISTINCT id) from names group by firstname having 2!=SUM(DISTINCT id) order by 1;
select 1+SUM(DISTINCT id) from names group by firstname order by 1+SUM(DISTINCT id);
select 1+SUM(DISTINCT id) from names group by firstname having 16=1+SUM(DISTINCT id)order by 1+SUM(DISTINCT id);

select SUM(DISTINCT 1+id) from names;
select SUM(DISTINCT 1+id) from names;
select SUM(DISTINCT 1+id) from names group by 1+id order by 1+id;
select SUM(DISTINCT 1+id) from names group by 1+id order by 1;
select SUM(DISTINCT 1+id) from names group by 1+id;
select SUM(DISTINCT 1+id) from names group by lastname;
select SUM(id) from names group by firstname,id;
select SUM(id) from names group by firstname;


select AVG(DISTINCT id) from names;
select 1 from names group by 1 having 1!=AVG(DISTINCT id);
select 1 from names group by 1 having 1!=AVG(DISTINCT id) order by AVG(DISTINCT id);
select 1 from names group by 1 order by AVG(DISTINCT id);
select 1 from names group by firstname having 1!=AVG(DISTINCT id);
select 1 from names group by firstname having 1!=AVG(DISTINCT id) order by AVG(DISTINCT id);
select 1 from names group by firstname order by AVG(DISTINCT id);
select AVG(DISTINCT id) from names group by firstname having 2!=AVG(DISTINCT id) order by AVG(DISTINCT id);
select AVG(DISTINCT id) from names group by firstname having 2!=AVG(DISTINCT id) order by 1;
select 1+AVG(DISTINCT id) from names group by firstname order by 1+AVG(DISTINCT id); -- sort-needed-check
select 1+AVG(DISTINCT id) from names group by firstname having 16=1+AVG(DISTINCT id)order by 1+AVG(DISTINCT id);

select AVG(DISTINCT 1+id) from names;
select AVG(DISTINCT 1+id) from names group by 1+id order by 1+id;
select AVG(DISTINCT 1+id) from names group by 1+id order by 1;
select AVG(DISTINCT 1+id) from names group by 1+id;
select AVG(DISTINCT 1+id) from names group by lastname;

select 1 from names group by 1 having 1!=count(lastname);
select 1 from names group by 1 having 1!=count(lastname) order by count(lastname);
select 1 from names group by 1 order by count(lastname);
select 1 from names group by firstname having 1!=count(lastname);
select 1 from names group by firstname having 1!=count(lastname) order by count(lastname);
select 1 from names group by firstname order by count(lastname);
select count(lastname) from names group by firstname having 0!=count(lastname) order by count(lastname);
select count(lastname) from names group by firstname having 0!=count(lastname) order by 1;
select 1+count(lastname) from names group by firstname order by 1+count(lastname);
select 1+count(lastname) from names group by firstname having 2=1+count(lastname)order by 1+count(lastname);

select count('hello'||lastname) from names;
select count('hello'||lastname) from names;
select count('hello'||lastname) from names group by 'hello'||lastname order by 'hello'||lastname;
select count('hello'||lastname) from names group by 'hello'||lastname order by 1;
select count('hello'||lastname) from names group by 'hello'||lastname;
select count('hello'||lastname) from names group by lastname;


select MAX(lastname) from names;
select 1 from names group by 1 having 'Burn'!=MAX(lastname);
select 1 from names group by 1 having 'Burn'!=MAX(lastname) order by MAX(lastname);
select 1 from names group by 1 order by MAX(lastname);
select 1 from names group by firstname having 'Burn'!=MAX(lastname);
select 1 from names group by firstname having 'Burn'!=MAX(lastname) order by MAX(lastname);
select 1 from names group by firstname order by MAX(lastname);
select MAX(lastname) from names group by firstname having 'Killer'!=MAX(lastname) order by MAX(lastname);
select MAX(lastname) from names group by firstname having 'Killer'!=MAX(lastname) order by 1;
select 'test'||MAX(lastname) from names group by firstname order by 'test'||MAX(lastname);
select 'test'||MAX(lastname) from names group by firstname having 'Cooltest'='test'||MAX(lastname)order by 'test'||MAX(lastname);

select MAX('hello'||lastname) from names;
select MAX('hello'||lastname) from names;
select MAX('hello'||lastname) from names group by 'hello'||lastname order by 'hello'||lastname;
select MAX('hello'||lastname) from names group by 'hello'||lastname order by 1;
select MAX('hello'||lastname) from names group by 'hello'||lastname;
select MAX('hello'||lastname) from names group by lastname;

select MIN(lastname) from names;
select 1 from names group by 1 having 'Burn'!=MIN(lastname);
select 1 from names group by 1 having 'Burn'!=MIN(lastname) order by MIN(lastname);
select 1 from names group by 1 order by MIN(lastname);
select 1 from names group by firstname having 'Burn'!=MIN(lastname);
select 1 from names group by firstname having 'Burn'!=MIN(lastname) order by MIN(lastname);
select 1 from names group by firstname order by MIN(lastname);
select MIN(lastname) from names group by firstname having 'Killer'!=MIN(lastname) order by MIN(lastname);
select MIN(lastname) from names group by firstname having 'Killer'!=MIN(lastname) order by 1;
select 'test'||MIN(lastname) from names group by firstname order by 'test'||MIN(lastname);
select 'test'||MIN(lastname) from names group by firstname having 'Cooltest'='test'||MIN(lastname)order by 'test'||MIN(lastname);

select MIN('hello'||lastname) from names;
select MIN('hello'||lastname) from names;
select MIN('hello'||lastname) from names group by 'hello'||lastname order by 'hello'||lastname;
select MIN('hello'||lastname) from names group by 'hello'||lastname order by 1;
select MIN('hello'||lastname) from names group by 'hello'||lastname;
select MIN('hello'||lastname) from names group by lastname;


select SUM(id) from names;
select 1 from names group by 1 having 1!=SUM(id);
select 1 from names group by 1 having 1!=SUM(id) order by SUM(id);
select 1 from names group by 1 order by SUM(id);
select 1 from names group by firstname having 1!=SUM(id);
select 1 from names group by firstname having 1!=SUM(id) order by SUM(id);
select 1 from names group by firstname order by SUM(id);
select SUM(id) from names group by firstname having 2!=SUM(id) order by SUM(id);
select SUM(id) from names group by firstname having 2!=SUM(id) order by 1;
select 1+SUM(id) from names group by firstname order by 1+SUM(id);
select 1+SUM(id) from names group by firstname having 16=1+SUM(id)order by 1+SUM(id);

select SUM(1+id) from names;
select SUM(1+id) from names;
select SUM(1+id) from names group by 1+id order by 1+id;
select SUM(1+id) from names group by 1+id order by 1;
select SUM(1+id) from names group by 1+id;
select SUM(1+id) from names group by lastname;
select SUM(id) from names group by firstname,id;
select SUM(id) from names group by firstname;


select AVG(id) from names;
select 1 from names group by 1 having 1!=AVG(id);
select 1 from names group by 1 having 1!=AVG(id) order by AVG(id);
select 1 from names group by 1 order by AVG(id);
select 1 from names group by firstname having 1!=AVG(id);
select 1 from names group by firstname having 1!=AVG(id) order by AVG(id);
select 1 from names group by firstname order by AVG(id);
select AVG(id) from names group by firstname having 2!=AVG(id) order by AVG(id);
select AVG(id) from names group by firstname having 2!=AVG(id) order by 1;
select 1+AVG(id) from names group by firstname order by 1+AVG(id); -- sort-needed-check
select 1+AVG(id) from names group by firstname having 16=1+AVG(id)order by 1+AVG(id);

select AVG(1+id) from names;
select AVG(1+id) from names group by 1+id order by 1+id;
select AVG(1+id) from names group by 1+id order by 1;
select AVG(1+id) from names group by 1+id;
select AVG(1+id) from names group by lastname;

select ARRAY(select firstname from names) from names group by firstname;
select ARRAY(select firstname from names) from names group by lastname;
select ARRAY(select firstname from names) from names group by lastname having ARRAY(select firstname from names)=ARRAY(select firstname from names);
select ARRAY(select firstname from names) from names group by lastname having ARRAY(select firstname from names)=ARRAY(select firstname from names) order by ARRAY(select firstname from names);
select ARRAY(select firstname from names) from names group by lastname having ARRAY(select firstname from names)=ARRAY(select firstname from names) order by lastname;
SELECT lastname FROM names GROUP BY 1 HAVING ARRAY(select firstname from names)=ARRAY(select firstname from names) ORDER BY 1;
select array(select 1 union select 2) from names group by firstname;
select array(VALUES(1)) from names group by firstname;

-- Subquery
-- All subquery usage within GroupBy should issue an unsupported error (checked by TGB25)
-- The following checks mostly the syntax of a basic subquery usage in the presence of a basic GroupBy column
select (select firstname from names limit 1) from names group by firstname;
select (select firstname from names limit 1) from names group by lastname;
select (select firstname from names limit 1) from names group by lastname having (select firstname from names limit 1)=(select firstname from names limit 1);
select (select firstname from names limit 1) from names group by lastname having (select firstname from names limit 1)=(select firstname from names limit 1) order by (select firstname from names limit 1);
select (select firstname from names limit 1) from names group by lastname having (select firstname from names limit 1)=(select firstname from names limit 1) order by lastname;
select (select firstname from names limit 1) from names group by lastname having (select lastname from names limit 1)=lastname order by lastname;
SELECT lastname FROM names GROUP BY 1 HAVING (select firstname from names limit 1)=(select firstname from names limit 1) ORDER BY 1;

select exists (select firstname from names limit 1) from names group by firstname;
select exists (select firstname from names limit 1),lastname from names group by lastname;
select exists (select firstname from names limit 1) from names group by lastname having exists (select firstname from names limit 1)=TRUE;
select exists (select firstname from names limit 1) from names group by lastname having exists (select firstname from names limit 1)=TRUE order by exists (select firstname from names limit 1);
select exists (select firstname from names limit 1) from names group by lastname having exists (select firstname from names limit 1) order by lastname;
SELECT lastname FROM names GROUP BY 1 HAVING exists (select firstname from names limit 1) ORDER BY 1;

-- quantified_comparison_predicate
-- The following checks mostly the syntax of a basic subquery usage with groupby
select 'Zero'!=ANY(select firstname from names limit 1) from names group by firstname;
select 'Zero'!=ANY(select firstname from names limit 1) from names group by lastname;
select 'Zero'!=ANY(select firstname from names limit 1) from names group by lastname having 'Zero'!=ANY(select firstname from names limit 1);
select 'Zero'!=ANY(select firstname from names limit 1) from names group by lastname having 'Zero'!=ANY(select firstname from names limit 1) order by 'Zero'!=ANY(select firstname from names limit 1);
select 'Zero'!=ANY(select firstname from names limit 1) from names group by lastname having 'Zero'!=ANY(select firstname from names limit 1) order by lastname;
select 'Zero'!=ANY(select firstname from names limit 1) from names group by lastname having lastname!=ANY(select lastname from names limit 1) order by lastname;

-- Between operation
-- between_predicate -> x between comparison_predicate AND comparison_predicate
--                   -> x not between comparison_predicate and comparison_predicate
SELECT id between 0 and 4 FROM names GROUP BY id between 0 and 4;
select id between 0 and 4 from names group by id between 0 and 4, id between 1 and 4 having id between 0 and 4 order by id between 1 and 4;
select id between 0 and 4 from names group by id between 0 and 4, id having id between 0 and 4 order by id between 1 and 4;
select id between id and id+1 from names group by id between id and id+1;
select id between id and id+1 from names group by id between id and id+1 having id between id and id+1;
select id between id and id+1 from names group by id between id and id+1 order by id between id and id+1;
select id between id and id+1 from names group by id order by id between id and id+2;
SELECT id between 0 and 4 FROM names GROUP BY 1;
select id between 0 and 4 from names group by 1, id between 1 and 4 having id between 0 and 4 order by id between 1 and 4;
select id between 0 and 4 from names group by 1, id having id between 0 and 4 order by id between 1 and 4;
select id between id and id+1 from names group by 1;
select id between id and id+1 from names group by 1 having id between id and id+1;
select id between id and id+1 from names group by 1 order by id between id and id+1;
SELECT 1 FROM names GROUP BY id between 0 and 4 ORDER BY id between 0 and 4;
select 1 between 0 and 4 from names group by id;
select 1 between 0 and 4 from names group by 1;
select 1 between 0 and 4 from names group by 1,id order by id;
select 1 between 0 and 4 from names group by 1,lastname having lastname!='Cool';
select 1 between 0 and 4 from names group by 1 order by 1;
select 1 between 0 and 4 from names group by 1 between 0 and 4 order by 1 between 1 and 4;

-- NOT between operation
SELECT id not between 0 and 4 FROM names GROUP BY id not between 0 and 4;
select id not between 0 and 4 from names group by id not between 0 and 4, id not between 1 and 4 having id not between 0 and 4 order by id not between 1 and 4;
select id not between 0 and 4 from names group by id not between 0 and 4, id having id not between 0 and 4 order by id not between 1 and 4;
select id not between id and id+1 from names group by id not between id and id+1;
select id not between id and id+1 from names group by id not between id and id+1 having id not between id and id+1;
select id not between id and id+1 from names group by id not between id and id+1 order by id not between id and id+1;
select id not between id and id+1 from names group by id order by id not between id and id+2;
SELECT id not between 0 and 4 FROM names GROUP BY 1;
select id not between 0 and 4 from names group by 1, id not between 1 and 4 having id not between 0 and 4 order by id not between 1 and 4;
select id not between 0 and 4 from names group by 1, id having id not between 0 and 4 order by id not between 1 and 4;
select id not between id and id+1 from names group by 1;
select id not between id and id+1 from names group by 1 having id not between id and id+1;
select id not between id and id+1 from names group by 1 order by id not between id and id+1;
SELECT 1 FROM names GROUP BY id not between 0 and 4 ORDER BY id not between 0 and 4;
select 1 not between 0 and 4 from names group by id;
select 1 not between 0 and 4 from names group by 1;
select 1 not between 0 and 4 from names group by 1,id order by id;
select 1 not between 0 and 4 from names group by 1,lastname having lastname!='Cool';
select 1 not between 0 and 4 from names group by 1 order by 1;
select 1 not between 0 and 4 from names group by 1 not between 0 and 4 order by 1 not between 1 and 4;

-- IN operation
select lastname in ('Cool','Killer',NULL) from names;
select lastname in ('Cool','Killer',NULL) from names group by lastname;
select lastname in ('Cool','Killer',NULL) from names group by lastname in ('Cool','Killer',NULL);
select lastname in ('Cool','Killer',NULL) from names group by lastname in ('Cool','Killer',NULL) having lastname in ('Cool','Killer',NULL);
select lastname in ('Cool','Killer',NULL) from names group by lastname in ('Cool','Killer',NULL) order by lastname in ('Cool','Killer',NULL); -- sort-needed-check
select lastname in ('Cool','Killer',NULL) from names group by lastname in ('Cool','Killer',NULL),lastname order by lastname in ('Cool',NULL,'Killer');
select lastname in (firstname,'Killer',NULL) from names;
select lastname in (firstname,'Killer',NULL) from names group by lastname,firstname;
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,'Killer',NULL);
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,'Killer',NULL) having lastname in (firstname,'Killer',NULL);
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,'Killer',NULL) order by lastname in (firstname,'Killer',NULL); -- sort-needed-check
select lastname in ('Cool','Killer',NULL) from names group by 1;
select lastname in ('Cool','Killer',NULL) from names group by 1 having lastname in ('Cool','Killer',NULL);
select lastname in ('Cool','Killer',NULL) from names group by 1 order by lastname in ('Cool','Killer',NULL); -- sort-needed-check
select lastname in ('Cool','Killer',NULL) from names group by 1,lastname order by lastname in ('Cool',NULL,'Killer');
select lastname in (firstname,'Killer',NULL) from names group by 1;
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,'Killer',NULL) having lastname in (firstname,'Killer',NULL);
select lastname in (firstname,'Killer',NULL) from names group by 1 order by lastname in (firstname,'Killer',NULL); -- sort-needed-check
select lastname in (firstname,'Killer',NULL) from names group by 1 order by 1; -- sort-needed-check
select lastname in (firstname,'Killer',NULL) from names group by 1 having lastname in (firstname, 'Killer',NULL) order by 1;
select 1 from names group by lastname in ('Cool','Killer',NULL) having lastname in ('Cool','Killer',NULL);
select 1 from names group by lastname in ('Cool','Killer',NULL) order by lastname in ('Cool','Killer',NULL);
select 1 from names group by lastname in ('Cool','Killer',NULL),lastname order by lastname in ('Cool',NULL,'Killer');
select 1 from names group by lastname in ('Cool','Killer',NULL);
select 1 from names group by lastname in (firstname,'Killer',NULL) having lastname in (firstname,'Killer',NULL);
select 1 from names group by lastname in (firstname,'Killer',NULL) order by lastname in (firstname,'Killer',NULL);
select 1 from names group by lastname in (firstname,'Killer',NULL) having lastname in (firstname, 'Killer',NULL) order by lastname in (firstname,'Killer',NULL);
select firstname from names group by lastname in (firstname,'Killer',NULL),firstname;
select firstname from names group by lastname,firstname order by lastname in (firstname,'Killer',NULL); -- sort-needed-check
select firstname from names group by lastname,firstname order by firstname in ('Killer',firstname,NULL); -- sort-needed-check
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,'Killer',NULL),lastname,firstname order by lastname in (firstname,NULL,'Killer');
select 1 from names group by lastname in (firstname,'Killer',NULL),lastname,firstname order by lastname in (firstname,NULL,'Killer');

-- INoperation with subquery
-- All of the queries with in subquery usage in group by should result in error
select lastname in (select 'Cool') from names;
select lastname in (select 'Cool') from names group by lastname;
select lastname in (select lastname from names) from names group by lastname,firstname;
select firstname from names group by lastname,firstname order by lastname in (select lastname from names); -- sort-needed-check
select firstname from names group by lastname,firstname order by firstname in (select firstname from names); -- sort-needed-check
select lastname in (select lastname from names) from names group by lastname;

-- CAST expression
SELECT CAST('12' AS INTEGER) FROM names GROUP BY CAST('12' AS INTEGER);
SELECT CAST('12' AS INTEGER) FROM names GROUP BY CAST('12' AS INTEGER) HAVING CAST('12' AS INTEGER)=12;
SELECT CAST('12' AS INTEGER) FROM names GROUP BY CAST('12' AS INTEGER) ORDER BY CAST('12' AS INTEGER);
SELECT CAST('12' AS INTEGER) FROM names GROUP BY CAST('12' AS INTEGER) HAVING CAST('12' AS INTEGER)=12 ORDER BY CAST('12' AS INTEGER);
SELECT CAST('12' AS INTEGER) FROM names GROUP BY 1;
SELECT CAST('12' AS INTEGER) FROM names GROUP BY 1 HAVING CAST('12' AS INTEGER)=12;
SELECT CAST('12' AS INTEGER) FROM names GROUP BY 1 HAVING CAST('12' AS INTEGER)=12 ORDER BY CAST('12' AS INTEGER);
SELECT CAST('12' AS INTEGER) FROM names GROUP BY 1 HAVING CAST('12' AS INTEGER)=12 ORDER BY 1;
SELECT CAST('12' AS INTEGER) FROM names GROUP BY firstname HAVING CAST('12' AS INTEGER)=12 ORDER BY 1;
SELECT CAST('12' AS INTEGER) FROM names GROUP BY 1,firstname HAVING CAST('12' AS INTEGER)=12 ORDER BY firstname;

SELECT CAST(id AS NUMERIC) FROM names GROUP BY CAST(id AS NUMERIC);
SELECT CAST(id AS NUMERIC) FROM names GROUP BY CAST(id AS NUMERIC) HAVING CAST(id AS NUMERIC)=12;
SELECT CAST(id AS NUMERIC) FROM names GROUP BY CAST(id AS NUMERIC) ORDER BY CAST(id AS NUMERIC);
SELECT CAST(id AS NUMERIC) FROM names GROUP BY CAST(id AS NUMERIC) HAVING CAST(id AS NUMERIC)=12 ORDER BY CAST(id AS NUMERIC);
SELECT CAST(id AS NUMERIC) FROM names GROUP BY 1;
SELECT CAST(id AS NUMERIC) FROM names GROUP BY 1 HAVING CAST(id AS NUMERIC)=3.0;
SELECT CAST(id AS NUMERIC) FROM names GROUP BY 1 HAVING CAST(id AS NUMERIC)=3.0 ORDER BY CAST(id AS NUMERIC);
SELECT CAST(id AS NUMERIC) FROM names GROUP BY 1 HAVING CAST(id AS NUMERIC)=3.0 ORDER BY 1;
SELECT CAST(id AS NUMERIC) FROM names GROUP BY firstname,id HAVING CAST(id AS NUMERIC)=3.0 ORDER BY 1;
SELECT CAST(id AS NUMERIC) FROM names GROUP BY 1,firstname HAVING CAST(id AS NUMERIC)=3.0 ORDER BY firstname;

-- With CAST_SPECIFICATION
select id::NUMERIC from names;
select id::NUMERIC from names group by id;
select id::NUMERIC from names group by id::NUMERIC;
select id::NUMERIC from names group by id having 1=id::NUMERIC;
select id::NUMERIC from names group by id having 1=id::NUMERIC order by id::NUMERIC;
select id::NUMERIC from names group by id having 1=id;
select id::NUMERIC from names group by id having 1=id::NUMERIC order by id;
select id::NUMERIC from names group by id::NUMERIC having 1=id::NUMERIC;
select id::NUMERIC from names group by id::NUMERIC having 1=id::NUMERIC order by id::NUMERIC;
select 1 from names group by id having 1=id::NUMERIC;
select 1 from names group by id having 1=id::NUMERIC order by id::NUMERIC;
select 1 from names group by id having 1=id;
select 1 from names group by id having 1=id::NUMERIC order by id;
select 1 from names group by id::NUMERIC having 1=id::NUMERIC;
select 1 from names group by id::NUMERIC having 1=id::NUMERIC order by id::NUMERIC;
select id::NUMERIC from names group by 1;
select id::NUMERIC from names group by 1 order by id::NUMERIC;
select id::NUMERIC from names group by 1 having 1!=id::NUMERIC order by id::NUMERIC;
select id::NUMERIC from names group by 1 order by 1;
select id::NUMERIC from names group by 1 having 1!=id::NUMERIC order by 1;
select id::NUMERIC from names group by id::NUMERIC,id;

-- CAST_EXPRESSION usage
select CAST(id AS NUMERIC) from names;
select CAST(id AS NUMERIC) from names group by id;
select CAST(id AS NUMERIC) from names group by CAST(id AS NUMERIC);
select CAST(id AS NUMERIC) from names group by id having 1=CAST(id AS NUMERIC);
select CAST(id AS NUMERIC) from names group by id having 1=CAST(id AS NUMERIC) order by CAST(id AS NUMERIC);
select CAST(id AS NUMERIC) from names group by id having 1=id;
select CAST(id AS NUMERIC) from names group by id having 1=CAST(id AS NUMERIC) order by id;
select CAST(id AS NUMERIC) from names group by CAST(id AS NUMERIC) having 1=CAST(id AS NUMERIC);
select CAST(id AS NUMERIC) from names group by CAST(id AS NUMERIC) having 1=CAST(id AS NUMERIC) order by CAST(id AS NUMERIC);
select 1 from names group by id having 1=CAST(id AS NUMERIC);
select 1 from names group by id having 1=CAST(id AS NUMERIC) order by CAST(id AS NUMERIC);
select 1 from names group by id having 1=id;
select 1 from names group by id having 1=CAST(id AS NUMERIC) order by id;
select 1 from names group by CAST(id AS NUMERIC) having 1=CAST(id AS NUMERIC);
select 1 from names group by CAST(id AS NUMERIC) having 1=CAST(id AS NUMERIC) order by CAST(id AS NUMERIC);
select CAST(id AS NUMERIC) from names group by 1;
select CAST(id AS NUMERIC) from names group by 1 order by CAST(id AS NUMERIC);
select CAST(id AS NUMERIC) from names group by 1 having 1!=CAST(id AS NUMERIC) order by CAST(id AS NUMERIC);
select CAST(id AS NUMERIC) from names group by 1 order by 1;
select CAST(id AS NUMERIC) from names group by 1 having 1!=CAST(id AS NUMERIC) order by 1;
select CAST(id AS NUMERIC) from names group by CAST(id AS NUMERIC),id;

-- COUNT(*)
select count(*);
select count(*) from names;
select 1 from names group by 1 having 1!=count(*);
select 1 from names group by 1 having 1!=count(*) order by count(*);
select 1 from names group by 1 order by count(*);
select 1 from names group by firstname having 1!=count(*);
select 1 from names group by firstname having 1!=count(*) order by count(*);
select 1 from names group by firstname order by count(*);
select count(*) from names group by firstname having 0!=count(*) order by count(*);
select count(*) from names group by firstname having 0!=count(*) order by 1;
select 1+count(*) from names group by firstname order by 1+count(*);
select 1+count(*) from names group by firstname having 2=1+count(*)order by 1+count(*);

-- TABLE_ASTERISK
SELECT 1 FROM names n1 GROUP BY n1.*;
select n1.* from names n1 group by 1,2,3;
select n1.*,n1.* from names n1 group by 1,2,3,4,5,6;
SELECT n1.* FROM names n1 ORDER BY n1.*;
SELECT n1.* FROM names n1 ORDER BY 1;
SELECT 1 FROM names n1 GROUP BY n1.* HAVING n1.* is null;
SELECT 1 FROM names n1 GROUP BY n1.* HAVING n1.* is not null;

-- misc
-- Similar to the query in #457
select lastname, EXISTS (SELECT alias1.lastName FROM names alias1 GROUP BY alias1.lastName) from names GROUP BY 1;
select lastname, EXISTS (SELECT 1 FROM names alias1 GROUP BY 'test'||alias1.lastName) from names GROUP BY 1;
select 'test'||n2.lastname, EXISTS (SELECT 'test'||n2.lastname FROM names alias1 GROUP BY 'test'||alias1.lastname) from names n2 GROUP BY 1,n2.lastname;
select 'test'||n2.lastname, EXISTS (SELECT n2.lastname FROM names alias1 GROUP BY 'test'||alias1.lastname) from names n2 GROUP BY 1,n2.lastname;
