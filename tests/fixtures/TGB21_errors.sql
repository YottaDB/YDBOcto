#################################################################
#								#
# Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Basic test
select * from names group by 1;
select n1.* from names n1 group by 1;

-- Following three queries check if literal in an expression present in GROUP BY and SELECT list is different
-- then the query errors out.
select lastname = 'Zero' from names group by lastname = 'Cool';
select lastname = 'Zero', count(lastname) from names group by lastname = 'Cool';
select TRUE!=((lastname='Cool')!=TRUE) from names group by TRUE!=((lastname='Zero')!=TRUE);
-- Following query checks if an error is thrown when expression is present reversed in GroupBy
select 'Cool' = lastname, count(lastname) from names group by lastname = 'Cool';

-- The following query demonstrates that if a column in select column list is not present
--  in GROUP BY as a individual node we through an error
select firstname from names group by firstname!='Cool';

-- Following query will generate error as expected.
-- Reason: `lastname` present in SELECT list is not present in GroupBy
select * from names group by id,firstname,1+id;

-- Constants other than INTEGER is not allowed similar to postgres
select 1 + 0 from names group by 'Zero';
select 'Zero' from names group by 'Zero';

-- The following queries generate error because expression or column in SELECT LIST is not present in GroupBy
select firstname = 'Zero', count(firstname) from names group by not firstname = 'Zero';
select id from names group by id not in (1,2);
select firstname is null,count(firstname) from names group by lastname is null;
select firstname from names group by firstname not in ('Cool','Zero');

-- Following queries validate that queries with multiple occurences of same node in GroupBy work fine
select 1+1 from names group by 1,1,1,2;

-- All the below queries are expected to throw errors as neither the columns
-- nor sub-expressions corresponding to both columns nor the entire expression holding both the columns
-- in SELECT list is present in GroupBy
select firstname != lastname from names group by firstname;
select ('Cool' != firstname)=('Zero' != lastname) from names group by firstname;
select ('Cool' != firstname)=('Zero' != lastname) from names group by 'Cool'!=firstname;
select firstname != lastname from names group by lastname != firstname;

-- Below query used to generate an assert failure in hash_canonical_query() call from qualify_statement()
SELECT id,firstname FROM names n1 WHERE id IN (SELECT 1!=(n1.id=0) FROM names n2 group by 1!=(n1.id=0::int));

-- Below queries test that ERR_TYPE_MISMATCH in Boolean operation is caught in GroupBy
select firstname=id from names group by firstname=id;
-- This query will throw a ERR_GROUP_BY_OR_AGGREGATE_FUNCTION as the select column expression or the participating columns are not in GroupBy.
select firstname=id from names group by firstname=1;
-- This query shows that if a query get past the expression related GroupBy validation the checks in populate_data_type()
-- are still performed and will result in any ERR_TYPE_MISMATCH errors.
select firstname=1 from names group by firstname=1;

-- Below queries test that aggregate functions are not allowed in GroupBy
select count(n1.*) from names as n1 group by count(n1.*);
select count(n1.*) from names as n1 group by 1;
select (1+count(id)) from names group by 1;
select count(n1.*) from names as n1 group by 1!=avg(n1.id);
select 1 from names as n1 group by 1!=count(n1.firstname);

-- Below queries refer to an invalid column number in GroupBy
select firstname from names group by 2;

-- Below queries test CASE statements
select CASE n1.id+1 WHEN 2 THEN 1 END  from names n1 group by n1.id+2 having CASE n1.id+1 WHEN 2 THEN TRUE END;
select count(n1.*) from names n1 group by n1.id+2 having CASE n1.id+1 WHEN 2 THEN TRUE END;
select count(n1.*) from names n1 group by n1.id+2 having CASE count(n1.id)+id WHEN 2 THEN TRUE END;
select 1 from names n1 group by n1.id+2 having CASE 2 WHEN n1.id+1 THEN TRUE END;
select CASE n1.id+1 WHEN 2 THEN 1 END  from names n1 group by 1 having CASE n1.id+1 WHEN 2 THEN TRUE END;
select CASE n1.id+1 WHEN 2 THEN 1 END  from names n1 group by CASE n1.id+1 WHEN 2 THEN 1 END having CASE n1.id+1 WHEN 2 THEN TRUE END;

-- Extensive variations
-- literal_value
-- -- STRING_LITERAL
SELECT 'test' FROM names GROUP BY 'test'; -- non integer constants in GroupBy are not allowed
SELECT 'test' FROM names GROUP BY 'test' HAVING 'test'!='hello'; -- non integer constants in GroupBy are not allowed
SELECT 'test' FROM names GROUP BY 'test' ORDER BY 'test'; -- non integer constants in GroupBy are not allowed
SELECT 'test' FROM names GROUP BY 1 ORDER BY 'test'; -- non integer constants in Order by are not allowed
SELECT 'test' FROM names GROUP BY 1 HAVING 'test'!='hello' ORDER BY 'test'; -- non integer constants in Order by are not allowed
SELECT 'test' FROM names GROUP BY firstname HAVING 'test'!='hello' ORDER BY 'test'; -- non integer constants in ORDER BY are not allowed
SELECT 'test' FROM names GROUP BY 1 HAVING 'test'!='hello' ORDER BY firstname; -- errors out saying firstname in ORDER BY must be in GROUP BY

-- -- NULL
SELECT NULL FROM names GROUP BY NULL; -- non integer constants in GroupBy are not allowed
SELECT NULL FROM names GROUP BY NULL HAVING NULL!='hello'; -- non integer constants in GroupBy are not allowed
SELECT NULL FROM names GROUP BY NULL ORDER BY NULL; -- non integer constants in ORDER BY are not allowed
SELECT NULL FROM names GROUP BY 1 ORDER BY NULL; -- non integer constants in Order by are not allowed
SELECT NULL FROM names GROUP BY 1 HAVING NULL!='hello' ORDER BY NULL; -- non integer constants in Order by are not allowed
SELECT NULL FROM names GROUP BY firstname HAVING NULL!='hello' ORDER BY NULL; -- non integer constants in Order by are not allowed
SELECT NULL FROM names GROUP BY 1 HAVING NULL!='hello' ORDER BY firstname; -- errors out saying firstname in ORDER BY must be in GROUP BY

-- TRUE
SELECT TRUE FROM names GROUP BY 1 HAVING TRUE!=FALSE ORDER BY firstname; -- errors out saying firstname in ORDER BY must be in GROUP BY
SELECT TRUE FROM names GROUP BY TRUE;
SELECT TRUE FROM names GROUP BY TRUE HAVING TRUE!=FALSE;
SELECT TRUE FROM names GROUP BY TRUE ORDER BY TRUE;
SELECT TRUE FROM names GROUP BY 1 ORDER BY TRUE;
SELECT TRUE FROM names GROUP BY 1 HAVING TRUE!=FALSE ORDER BY TRUE;
SELECT TRUE FROM names GROUP BY firstname HAVING TRUE!=FALSE ORDER BY TRUE;

-- null_if
SELECT nullif('soml','test','hello') FROM names; -- Postgres ERROR
SELECT nullif('soml') FROM names;       -- Postgres ERROR
SELECT nullif(NULL,'soml','loms') FROM names GROUP BY 1; -- Postgres ERROR

-- INTEGER_LITERAL
SELECT 12 FROM names GROUP BY 12; -- numbers in GROUP BY are treated as column numbers and not regular literal value
SELECT 12 FROM names GROUP BY 12 HAVING 12=12; -- Postgres loc error
SELECT 12 FROM names GROUP BY 12 ORDER BY 12; -- Postgres loc error
SELECT 12 FROM names GROUP BY 12 HAVING 12=12 ORDER BY 12; -- numbers in ORDER BY are treated as column numbers and not regular literal value
SELECT 12 FROM names GROUP BY 1 HAVING 12=12 ORDER BY 12; -- loc error
SELECT 12 FROM names GROUP BY 1 HAVING 12=12 ORDER BY firstname; -- errors out saying firstname in ORDER BY must be in GROUP BY

SELECT nullif(1) FROM names; -- Postgres error
SELECT nullif(NULL,5,4) FROM names GROUP BY 1; -- Postgres error

-- literal_value
-- -- NUMERIC_LITERAL
SELECT 1.2 FROM names GROUP BY 1.2; -- non-integer constants in GROUP BY are not supported
SELECT 1.2 FROM names GROUP BY 1.2 HAVING 1.2=1.2; -- non-integer constants in GROUP BY are not supported
SELECT 1.2 FROM names GROUP BY 1.2 ORDER BY 1.2; -- non-integer constants in ORDER BY are not supported
SELECT 1.2 FROM names GROUP BY 1.2 HAVING 1.2=1.2 ORDER BY 1.2; -- non-integer constants in ORDER BY are not supported
SELECT 1.2 FROM names GROUP BY 1 HAVING 1.2=1.2 ORDER BY 1.2;  -- non-integer constants in ORDER BY are not supported
SELECT 1.2 FROM names GROUP BY 1 HAVING 1.2=1.2 ORDER BY firstname; -- errors out saying firstname in ORDER BY must be in GROUP BY

SELECT greatest(5.5,4.3) FROM names GROUP BY greatest('test',4) ORDER BY 1; -- Postgres error saying invalid input syntax

SELECT least(5.5,4.3) FROM names GROUP BY least('test',4) ORDER BY 1; -- Postgres error saying invalid input syntax

-- literal_value
-- -- PARAMETER_VALUE
-- All of the following are error cases. They are here just to validate the error.
SELECT $1 FROM names GROUP BY 1;
SELECT 1 FROM names GROUP BY $1;
SELECT 1 FROM names GROUP BY 1 ORDER BY $1;
SELECT 1 FROM names GROUP BY 1 HAVING $1>0;

SELECT coalesce($1) FROM names;
SELECT 1 FROM names GROUP BY coalesce($1);
SELECT 1 FROM names GROUP BY 1 ORDER BY coalesce($1);
SELECT 1 FROM names GROUP BY 1 HAVING coalesce($1);

SELECT nullif($1,$2) FROM names;
SELECT 1 FROM names GROUP BY nullif($1,$2);
SELECT 1 FROM names GROUP BY 1 ORDER BY nullif($1,$2);
SELECT 1 FROM names GROUP BY 1 HAVING nullif($1,$2);

SELECT greatest($1,$2) FROM names;
SELECT 1 FROM names GROUP BY greatest($1,$2);
SELECT 1 FROM names GROUP BY 1 ORDER BY greatest($1,$2);
SELECT 1 FROM names GROUP BY 1 HAVING greatest($1,$2);

SELECT least($1,$2) FROM names;
SELECT 1 FROM names GROUP BY least($1,$2);
SELECT 1 FROM names GROUP BY 1 ORDER BY least($1,$2);
SELECT 1 FROM names GROUP BY 1 HAVING least($1,$2);

-- column_reference
-- -- IDENTIFIER_ALONE (column)
SELECT firstname FROM names GROUP BY firstname HAVING firstname!='COOL' ORDER BY lastname; -- ERROR as lastname not in GROUP BY
SELECT firstname FROM names GROUP BY firstname HAVING lastname!='COOL'; -- ERROR as lastname not in GROUP BY
SELECT lastname FROM names GROUP BY firstname; -- ERROR as lastname not in GROUP BY
SELECT lastname FROM names GROUP BY 1 HAVING firstname!='Cool' ORDER BY 1; -- ERROR firstname must appear in GROUP BY
SELECT firstname FROM names GROUP BY 2; -- Location error
SELECT firstname FROM names GROUP BY 1 HAVING firstname!='Cool' ORDER BY 2; -- location error
SELECT firstname FROM names GROUP BY 1 ORDER BY 2; -- ERROR ORDER BY position is wrong
SELECT firstname FROM names GROUP BY 2 ORDER BY 2; -- ERROR ORDER BY position is wrong

SELECT coalesce(lastname) FROM names GROUP BY coalesce(lastname) HAVING firstname!='COOL'; -- Postgres ERROR saying firstname not in GroupBy
SELECT coalesce(lastname) FROM names GROUP BY coalesce(lastname) HAVING coalesce(firstname)!='COOL'; -- Postgres error saying firstname not in GroupBy
SELECT coalesce(firstname) FROM names GROUP BY coalesce(lastname); -- Postgres error saying firstname must  be in GroupBy
SELECT coalesce(lastname) FROM names GROUP BY 1 HAVING coalesce(firstname)!='Cool'; -- Postgres ERROR saying firstname must be in GroupBy
SELECT coalesce(lastname) FROM names GROUP BY 1 ORDER BY coalesce(firstname); -- Postgres ERROR saying firstname must be in GroupBy
SELECT coalesce(lastname) FROM names GROUP BY 1 HAVING coalesce(firstname)!='COOL' ORDER BY 1; -- Posgres error saying firstname not in GroupBy
SELECT coalesce(firstname) FROM names GROUP BY 1 HAVING coalesce(lastname)!='COOL' ORDER BY 2; -- location error
SELECT coalesce(lastname) FROM names GROUP BY 1 ORDER BY 2; -- Postgres error location ORDER BY
SELECT coalesce(lastname) FROM names GROUP BY 2 ORDER BY 2; -- Postgres error location ORDER BY
SELECT coalesce(lastname) FROM names GROUP BY 2; -- Postgres error location group by

SELECT coalesce(lastname,firstname) FROM names GROUP BY coalesce(lastname,firstname) HAVING firstname!='COOL'; -- Postgres ERROR saying firstname not in GroupBy
SELECT coalesce(lastname,firstname) FROM names GROUP BY coalesce(lastname,firstname) HAVING coalesce(lastname,firstname)!='COOL' ORDER BY coalesce(lastname); -- Postgres error saying lastname not in Group By
SELECT coalesce(lastname,firstname) FROM names GROUP BY coalesce(lastname,firstname) HAVING coalesce(lastname)!='COOL'; -- Postgres error saying lastname not in GroupBy
SELECT coalesce(lastname) FROM names GROUP BY coalesce(lastname,firstname); -- Postgres error saying lastname FROM SELECT list must  be in GroupBy
SELECT coalesce(lastname,firstname) FROM names GROUP BY 1 HAVING coalesce(lastname)!='Cool'; -- Postgres ERROR saying lastname must be in GroupBy
SELECT coalesce(lastname,firstname) FROM names GROUP BY 1 ORDER BY coalesce(lastname); -- Postgres ERROR saying lastanem must be in GroupBy
SELECT coalesce(lastname) FROM names GROUP BY 1 HAVING coalesce(lastname,firstname)!='COOL' ORDER BY 1; -- Postgres error saying lastname in coalesce is not in GROUP BY
SELECT coalesce(lastname) FROM names GROUP BY 2; -- loc error group by
SELECT coalesce(lastname) FROM names GROUP BY 1 HAVING coalesce(lastname,firstname)!='COOL' ORDER BY 2; -- location error
SELECT coalesce(lastname) FROM names GROUP BY 1 ORDER BY 2; -- Posgres loc error order by
SELECT coalesce(lastname) FROM names GROUP BY 2 ORDER BY 2; -- Potgres loc error order by
SELECT coalesce(lastname,firstname) FROM names GROUP BY lastname; -- Postgres error saying firstname must be in GROUP BY
SELECT lastname,firstname FROM names GROUP BY coalesce(lastname,firstname); -- Postgres error saying lastname FROM SELECT list must be in groupby
SELECT 1 FROM names GROUP BY COALESCE(lastname,firstname,id); -- Postgres error saying `COALESCE types character varying and integer cannot be matched`
SELECT 1 FROM names GROUP BY COALESCE(lastname,firstname) HAVING coalesce(lastname)!='cool'; -- Postgres error lastname must appear in GROUP BY
SELECT 1 FROM names GROUP BY COALESCE(lastname,firstname) ORDER BY coalesce(lastname); -- Postgres error lastname must appear in GROUP BY
SELECT coalesce(lastname,firstname) FROM names GROUP BY coalesce(lastname,'test'); -- Postgres error SELECT list lastname not in GroupBy
SELECT coalesce(lastname,firstname) FROM names GROUP BY coalesce(lastname,NULL); -- Postgres error SELECT list lastname not in GroupBy
SELECT coalesce(lastname) FROM names GROUP BY 2; -- loc error group by
SELECT coalesce(lastname) FROM names GROUP BY 1 ORDER BY 2; -- loc error order by
SELECT coalesce(lastname) FROM names GROUP BY 2 ORDER BY 2; -- loc error order by

SELECT nullif(lastname,firstname) FROM names GROUP BY nullif(lastname,firstname) HAVING firstname!='COOL'; -- Postgres ERROR saying firstname not in GroupBy
SELECT nullif(lastname,firstname) FROM names GROUP BY nullif(lastname,firstname) HAVING nullif(lastname,firstname)!='COOL' ORDER BY nullif(lastname,lastname); -- Postgres error saying lastname not in Group By
SELECT nullif(lastname,firstname) FROM names GROUP BY nullif(lastname,firstname) HAVING nullif(lastname,lastname)!='COOL'; -- Postgres error saying lastname not in GroupBy
SELECT nullif(lastname,lastname) FROM names GROUP BY nullif(lastname,firstname); -- Postgres error saying lastname FROM SELECT list must  be in GroupBy
SELECT nullif(lastname,firstname) FROM names GROUP BY 1 HAVING nullif(lastname,lastname)!='Cool'; -- Postgres ERROR saying lastname must be in GroupBy
SELECT nullif(lastname,firstname) FROM names GROUP BY 1 ORDER BY nullif(lastname,lastname); -- Postgres ERROR saying lastanem must be in GroupBy
SELECT nullif(lastname,lastname) FROM names GROUP BY 1 HAVING nullif(lastname,firstname)!='COOL' ORDER BY 1; -- Postgres error saying lastname in coalesce is not in GROUP BY
SELECT nullif(lastname,lastname) FROM names GROUP BY 2; -- group by loc error
SELECT nullif(lastname,lastname) FROM names GROUP BY 1 HAVING nullif(lastname,firstname)!='COOL' ORDER BY 2; -- location error
SELECT nullif(lastname,lastname) FROM names GROUP BY 1 ORDER BY 2; -- order by loc error
SELECT nullif(lastname,lastname) FROM names GROUP BY 2 ORDER BY 2; -- order by loc error
SELECT nullif(lastname,firstname) FROM names GROUP BY lastname; -- Postgres error saying firstname must be in GROUP BY
SELECT lastname,firstname FROM names GROUP BY nullif(lastname,firstname); -- Postgres error saying lastname FROM SELECT list must be in groupby
SELECT 1 FROM names GROUP BY nullif(lastname,id); -- Postgres error saying `nullif types character varying`
SELECT 1 FROM names GROUP BY nullif(lastname,firstname) HAVING nullif(lastname,lastname)!='cool'; -- Postgres error lastname must appear in GROUP BY
SELECT 1 FROM names GROUP BY nullif(lastname,firstname) ORDER BY nullif(lastname,lastname); -- Postgres error lastname must appear in GROUP BY
SELECT nullif(lastname,firstname) FROM names GROUP BY nullif(lastname,'test'); -- Postgres error SELECT list lastname not in GroupBy
SELECT nullif(lastname,firstname) FROM names GROUP BY nullif(lastname,NULL); -- Postgres error SELECT list lastname not in GroupBy

SELECT greatest(lastname,firstname) FROM names GROUP BY greatest(lastname,firstname) HAVING firstname!='COOL'; -- Postgres ERROR saying firstname not in GroupBy
SELECT greatest(lastname,firstname) FROM names GROUP BY greatest(lastname,firstname) HAVING greatest(lastname,firstname)!='COOL' ORDER BY greatest(lastname,lastname); -- Postgres error saying lastname not in Group By
SELECT greatest(lastname,firstname) FROM names GROUP BY greatest(lastname,firstname) HAVING greatest(lastname,lastname)!='COOL'; -- Postgres error saying lastname not in GroupBy
SELECT greatest(lastname,lastname) FROM names GROUP BY greatest(lastname,firstname); -- Postgres error saying lastname FROM SELECT list must  be in GroupBy
SELECT greatest(lastname,firstname) FROM names GROUP BY 1 HAVING greatest(lastname,lastname)!='Cool'; -- Postgres ERROR saying lastname must be in GroupBy
SELECT greatest(lastname,firstname) FROM names GROUP BY 1 ORDER BY greatest(lastname,lastname); -- Postgres ERROR saying lastanem must be in GroupBy
SELECT greatest(lastname,lastname) FROM names GROUP BY 1 HAVING greatest(lastname,firstname)!='COOL' ORDER BY 1; -- Postgres error saying lastname in coalesce is not in GROUP BY
SELECT greatest(lastname,lastname) FROM names GROUP BY 2; -- postgres error group by loc
SELECT greatest(lastname,lastname) FROM names GROUP BY 1 HAVING greatest(lastname,firstname)!='COOL' ORDER BY 2; -- location error
SELECT greatest(lastname,lastname) FROM names GROUP BY 1 ORDER BY 2; -- Postgres error order by loc
SELECT greatest(lastname,lastname) FROM names GROUP BY 2 ORDER BY 2; -- postgres error order by loc
SELECT greatest(lastname,firstname) FROM names GROUP BY lastname; -- Postgres error saying firstname must be in GROUP BY
SELECT lastname,firstname FROM names GROUP BY greatest(lastname,firstname); -- Postgres error saying lastname FROM SELECT list must be in groupby
SELECT 1 FROM names GROUP BY greatest(lastname,id); -- Postgres error saying `greatest types character varying`
SELECT 1 FROM names GROUP BY greatest(lastname,firstname) HAVING greatest(lastname,lastname)!='cool'; -- Postgres error lastname must appear in GROUP BY
SELECT 1 FROM names GROUP BY greatest(lastname,firstname) ORDER BY greatest(lastname,lastname); -- Postgres error lastname must appear in GROUP BY
SELECT greatest(lastname,firstname) FROM names GROUP BY greatest(lastname,'test'); -- Postgres error SELECT list lastname not in GroupBy
SELECT greatest(lastname,firstname) FROM names GROUP BY greatest(lastname,NULL); -- Postgres error SELECT list lastname not in GroupBy

SELECT least(lastname,firstname) FROM names GROUP BY least(lastname,firstname) HAVING firstname!='COOL'; -- Postgres ERROR saying firstname not in GroupBy
SELECT least(lastname,firstname) FROM names GROUP BY least(lastname,firstname) HAVING least(lastname,firstname)!='COOL' ORDER BY least(lastname,lastname); -- Postgres error saying lastname not in Group By
SELECT least(lastname,firstname) FROM names GROUP BY least(lastname,firstname) HAVING least(lastname,lastname)!='COOL'; -- Postgres error saying lastname not in GroupBy
SELECT least(lastname,lastname) FROM names GROUP BY least(lastname,firstname); -- Postgres error saying lastname FROM SELECT list must  be in GroupBy
SELECT least(lastname,firstname) FROM names GROUP BY 1 HAVING least(lastname,lastname)!='Cool'; -- Postgres ERROR saying lastname must be in GroupBy
SELECT least(lastname,firstname) FROM names GROUP BY 1 ORDER BY least(lastname,lastname); -- Postgres ERROR saying lastanem must be in GroupBy
SELECT least(lastname,lastname) FROM names GROUP BY 1 HAVING least(lastname,firstname)!='COOL' ORDER BY 1; -- Postgres error saying lastname in coalesce is not in GROUP BY
SELECT least(lastname,lastname) FROM names GROUP BY 2; -- Postgres error group by loc
SELECT least(lastname,lastname) FROM names GROUP BY 1 HAVING least(lastname,firstname)!='COOL' ORDER BY 2; -- location error
SELECT least(lastname,lastname) FROM names GROUP BY 1 ORDER BY 2; -- error order by loc
SELECT least(lastname,lastname) FROM names GROUP BY 2 ORDER BY 2; -- error order by loc
SELECT least(lastname,firstname) FROM names GROUP BY lastname; -- Postgres error saying firstname must be in GROUP BY
SELECT lastname,firstname FROM names GROUP BY least(lastname,firstname); -- Postgres error saying lastname FROM SELECT list must be in groupby
SELECT 1 FROM names GROUP BY least(lastname,id); -- Postgres error saying `least types character varying`
SELECT 1 FROM names GROUP BY least(lastname,firstname) HAVING least(lastname,lastname)!='cool'; -- Postgres error lastname must appear in GROUP BY
SELECT 1 FROM names GROUP BY least(lastname,firstname) ORDER BY least(lastname,lastname); -- Postgres error lastname must appear in GROUP BY
SELECT least(lastname,firstname) FROM names GROUP BY least(lastname,'test'); -- Postgres error SELECT list lastname not in GroupBy
SELECT least(lastname,firstname) FROM names GROUP BY least(lastname,NULL); -- Postgres error SELECT list lastname not in GroupBy
select +id from names group by -id;
select -id from names group by +id;
select id from names group by +id;
select lastname from names group by not lastname;

-- column_reference
-- -- IDENTIFIER_PERIOD_IDENTIFIER (table.column)
SELECT n1.firstname FROM names n1 GROUP BY n1.firstname HAVING n1.firstname!='COOL' ORDER BY n1.lastname; -- ERROR as lastname not in GROUP BY
SELECT n1.firstname FROM names n1 GROUP BY n1.firstname HAVING n1.lastname!='COOL'; -- ERROR as lastname not in GROUP BY
SELECT n1.lastname FROM names n1 GROUP BY n1.firstname; -- ERROR as lastname not in GROUP BY
SELECT n1.lastname FROM names n1 GROUP BY 1 HAVING n1.firstname!='Cool' ORDER BY 1; -- ERROR n1.firstname must appear in GROUP BY
SELECT n1.firstname FROM names n1 GROUP BY 2; -- Postgres error group by loc
SELECT n1.firstname FROM names n1 GROUP BY 1 HAVING n1.firstname!='Cool' ORDER BY 2; -- postgres error order by loc
SELECT n1.firstname FROM names n1 GROUP BY 1 ORDER BY 2; -- ERROR ORDER BY position is wrong
SELECT n1.firstname FROM names n1 GROUP BY 2 ORDER BY 2; -- ERROR ORDER BY position is wrong

-- cas_STATEMENT
SELECT CASE WHEN 'te'||'st' THEN 'one' end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY CASE WHEN 'te'||'st' THEN 'one' end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY CASE WHEN 'te'||'st' THEN 'one' end ORDER BY CASE WHEN 'te'||'st' THEN 'one' end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY 1 HAVING 1=CASE WHEN 'te'||'st' THEN 'one' end ORDER BY 1; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY 1 HAVING 1=CASE WHEN 'te'||'st' THEN 'one' end ORDER BY CASE WHEN 'te'||'st' THEN 'one' end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`

SELECT CASE WHEN 3-1 THEN 1 end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type integer`
SELECT 1 FROM names GROUP BY CASE WHEN 3-1 THEN 1 end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type integer`
SELECT 1 FROM names GROUP BY CASE WHEN 3-1 THEN 1 end ORDER BY CASE WHEN 3-1 THEN 1 end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type integer`
SELECT 1 FROM names GROUP BY 1 HAVING 1=CASE WHEN 3-1 THEN 1 end ORDER BY 1; -- Postgres error `argument of CASE/WHEN must be type boolean, not type integer`
SELECT 1 FROM names GROUP BY 1 HAVING 1=CASE WHEN 3-1 THEN 1 end ORDER BY CASE WHEN 3-1 THEN 1 end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type integer`

SELECT CASE WHEN 1.1+1.1 THEN 1.1 end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY CASE WHEN 1.1+1.1 THEN 1.1 end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY CASE WHEN 1.1+1.1 THEN 1.1 end ORDER BY CASE WHEN 1.1+1.1 THEN 1.1 end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY 1 HAVING 1=CASE WHEN 1.1+1.1 THEN 1.1 end ORDER BY 1; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY 1 HAVING 1=CASE WHEN 1.1+1.1 THEN 1.1 end ORDER BY CASE WHEN 1.1+1.1 THEN 1.1 end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`

-- All of the below group will no parameter error in postgres
SELECT CASE $1 WHEN $1 THEN 1 WHEN 2+1 THEN 1 end;
SELECT 1 FROM names GROUP BY CASE $1 WHEN $1 THEN 1 WHEN 2+1 THEN 1 end;
SELECT 1 FROM names GROUP BY 1 ORDER BY CASE $1 WHEN $1 THEN 1 WHEN 2+1 THEN 1 end;
SELECT 1 FROM names GROUP BY 1 HAVING 1=CASE $1 WHEN $1 THEN 1 WHEN 2+1 THEN 1 end;

SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end; -- Postgres error firstname in select not in GroupBy
SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end HAVING 'Cool'!=CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end; -- Postgres error firstname in select not in GroupBy
SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end HAVING 'Cool'=CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end; -- Postgres error firstname in select not in GroupBy
SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end HAVING 'Cool'=CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end ORDER BY  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end; -- Postgres error firstname in select not in GroupBy
SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end HAVING 'Cool'=CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end ORDER BY CASE firstname WHEN firstname||lastname THEN lastname WHEN lastname THEN lastname end; -- Postgres error firstname in select not in GroupBy
SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end ORDER BY CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end; -- Postgres error firstname in select not in GroupBy
SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end ORDER BY 1; -- Postgres error firstname in select not in GroupBy
SELECT  CASE firstname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE firstname||'test' end FROM names GROUP BY 1 HAVING 'Cool'=CASE lastname WHEN firstname||lastname THEN lastname WHEN firstname THEN firstname ELSE lastname||'test' end ORDER BY 1; -- Postgres error lastname of having not in GroupBY
SELECT CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end FROM names GROUP BY  CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end HAVING 'Cool'=CASE WHEN NULL THEN firstname WHEN firstname||lastname=firstname THEN lastname ELSE firstname||'test' end ORDER BY CASE firstname WHEN firstname||lastname THEN lastname WHEN lastname THEN lastname end; -- Postgres error firstname in OrderBy not in GroupBy

SELECT CASE WHEN firstname||lastname THEN firstname end from names; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY CASE WHEN firstname||lastname THEN firstname end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY CASE WHEN firstname||lastname THEN firstname end ORDER BY CASE WHEN firstname||lastname THEN firstname end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY 1 HAVING 'Cool'=CASE WHEN firstname||lastname THEN firstname end ORDER BY 1; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`
SELECT 1 FROM names GROUP BY 1 HAVING 'Cool'=CASE WHEN firstname||lastname THEN firstname end; -- Postgres error `argument of CASE/WHEN must be type boolean, not type text`

select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY lastname; -- Postgres error firstname in select not in GroupBy
select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY CASE lastname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END; -- Postgres error firstname in select not in GroupBy
select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY 1 ORDER BY CASE lastname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END; -- Postgres error lastname in order by not in GroupBy
select CASE firstname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY 1 HAVING 'Cool'=CASE lastname WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END ORDER BY 1; -- Posgres error lastname in HAVING not in GroupBy

select CASE 1 WHEN 1 THEN firstname WHEN 2 THEN 2 ELSE 3 END FROM names GROUP BY firstname; -- Postgres error firstname in select list `CASE types integer and character varying cannot be matched`
select CASE 'Cool' WHEN 'Cool' THEN firstname WHEN 2 THEN 2 ELSE 3 END FROM names GROUP BY firstname; -- Postgres error `operator does not exist: text = integer`
select CASE 'Cool' WHEN 'Cool' THEN firstname WHEN 'Zero' THEN 'test' ELSE 'default' END FROM names GROUP BY lastname; -- Postgres error firstname in select not in GroupBy
select CASE 'Cool' WHEN 'Cool' THEN 'tset' WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname; -- Postgres error firstname in seelct not in GroupBy
select CASE 'Cool' WHEN 'Cool' THEN 'tset' WHEN lastname THEN 'test' ELSE firstname END FROM names GROUP BY firstname; -- Postgres error lastname in seelct not in GroupBy
select CASE 'Cool' WHEN 'Cool' THEN 'tset' WHEN lastname THEN 'test' ELSE firstname END FROM names GROUP BY lastname; -- Postgres error firstname in select not in GroupBy

select CASE 'Cool' WHEN 'Cool' THEN lastname WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY firstname; -- Postgres error lastname in seelct not in GroupBy
select CASE 'Cool' WHEN 'Cool' THEN lastname WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname; -- Postgres error firstname in select not in GroupBy
select CASE 'Cool' WHEN 'Cool' THEN lastname WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname,firstname ORDER BY CASE 1 WHEN 1 THEN id END; -- Postgres error id in ORder by must be in GroupBY
select CASE 'Cool' WHEN 'Cool' THEN lastname WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname,firstname ORDER BY 1+id; -- Postgres error id in Order By must be in GroupBy
select CASE 'Cool' WHEN 'Cool' THEN lastname||'test' WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname||'test',firstname ORDER BY lastname; -- Postgres error lastname in Order BY not in GroupBY
select CASE 'Cool' WHEN 'Cool' THEN lastname||'test' WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname||'test',firstname HAVING 1=id ORDER BY lastname||'test',firstname; -- Postgres error id in order by not in GroupBY
select CASE 'Cool' WHEN 'Cool' THEN lastname||'test' WHEN 'Zero' THEN 'test' ELSE firstname END FROM names GROUP BY lastname||'test',firstname HAVING 'Burn'=lastname ORDER BY lastname||'test',firstname; -- Postgres error as having has usage of lastname and not lastname||'test' which is the base expression in GroupBY

-- aggregate_functions

select count(DISTINCT lastname) from names group by count(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(DISTINCT lastname) from names group by count(DISTINCT lastname) having 1!=count(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(DISTINCT lastname) from names group by count(DISTINCT lastname) having 1!=count(DISTINCT lastname) order by count(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(DISTINCT lastname) from names group by count(DISTINCT lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(DISTINCT lastname) from names group by count(DISTINCT lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(DISTINCT lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(DISTINCT lastname) from names group by 1 having 1!=count(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(DISTINCT lastname) from names group by 1 having 1!=count(DISTINCT lastname) order by count(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(DISTINCT lastname) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(DISTINCT lastname) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(DISTINCT lastname) having 1!=count(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(DISTINCT lastname) having 1!=count(DISTINCT lastname) order by count(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(DISTINCT lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(DISTINCT lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 1+count(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1+count(DISTINCT lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

select MAX(DISTINCT lastname) from names group by MAX(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(DISTINCT lastname) from names group by MAX(DISTINCT lastname) having 'Burn'!=MAX(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(DISTINCT lastname) from names group by MAX(DISTINCT lastname) having 'Burn'!=MAX(DISTINCT lastname) order by MAX(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(DISTINCT lastname) from names group by MAX(DISTINCT lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(DISTINCT lastname) from names group by MAX(DISTINCT lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(DISTINCT lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(DISTINCT lastname) from names group by 1 having 'Burn'!=MAX(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(DISTINCT lastname) from names group by 1 having 'Burn'!=MAX(DISTINCT lastname) order by MAX(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(DISTINCT lastname) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(DISTINCT lastname) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(DISTINCT lastname) having 'Burn'!=MAX(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(DISTINCT lastname) having 'Burn'!=MAX(DISTINCT lastname) order by MAX(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(DISTINCT lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(DISTINCT lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 'test'||MAX(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 'test'||MAX(DISTINCT lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

select MIN(DISTINCT lastname) from names group by MIN(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(DISTINCT lastname) from names group by MIN(DISTINCT lastname) having 'Burn'!=MIN(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(DISTINCT lastname) from names group by MIN(DISTINCT lastname) having 'Burn'!=MIN(DISTINCT lastname) order by MIN(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(DISTINCT lastname) from names group by MIN(DISTINCT lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(DISTINCT lastname) from names group by MIN(DISTINCT lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(DISTINCT lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(DISTINCT lastname) from names group by 1 having 'Burn'!=MIN(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(DISTINCT lastname) from names group by 1 having 'Burn'!=MIN(DISTINCT lastname) order by MIN(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(DISTINCT lastname) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(DISTINCT lastname) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(DISTINCT lastname) having 'Burn'!=MIN(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(DISTINCT lastname) having 'Burn'!=MIN(DISTINCT lastname) order by MIN(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(DISTINCT lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(DISTINCT lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 'test'||MIN(DISTINCT lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 'test'||MIN(DISTINCT lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

select SUM(DISTINCT id) from names;
select SUM(DISTINCT id) from names group by SUM(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(DISTINCT id) from names group by SUM(DISTINCT id) having 1!=SUM(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(DISTINCT id) from names group by SUM(DISTINCT id) having 1!=SUM(DISTINCT id) order by SUM(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(DISTINCT id) from names group by SUM(DISTINCT id) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(DISTINCT id) from names group by SUM(DISTINCT id) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(DISTINCT id) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(DISTINCT id) from names group by 1 having 1!=SUM(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(DISTINCT id) from names group by 1 having 1!=SUM(DISTINCT id) order by SUM(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(DISTINCT id) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(DISTINCT id) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(DISTINCT id) having 1!=SUM(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(DISTINCT id) having 1!=SUM(DISTINCT id) order by SUM(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(DISTINCT id) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(DISTINCT id) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 1+SUM(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1+SUM(DISTINCT id) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

select AVG(DISTINCT id) from names group by AVG(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(DISTINCT id) from names group by AVG(DISTINCT id) having 1!=AVG(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(DISTINCT id) from names group by AVG(DISTINCT id) having 1!=AVG(DISTINCT id) order by AVG(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(DISTINCT id) from names group by AVG(DISTINCT id) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(DISTINCT id) from names group by AVG(DISTINCT id) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(DISTINCT id) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(DISTINCT id) from names group by 1 having 1!=AVG(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(DISTINCT id) from names group by 1 having 1!=AVG(DISTINCT id) order by AVG(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(DISTINCT id) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(DISTINCT id) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(DISTINCT id) having 1!=AVG(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(DISTINCT id) having 1!=AVG(DISTINCT id) order by AVG(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(DISTINCT id) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(DISTINCT id) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 1+AVG(DISTINCT id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1+AVG(DISTINCT id) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy


select count(lastname) from names group by count(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(lastname) from names group by count(lastname) having 1!=count(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(lastname) from names group by count(lastname) having 1!=count(lastname) order by count(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(lastname) from names group by count(lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(lastname) from names group by count(lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(lastname) from names group by 1 having 1!=count(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(lastname) from names group by 1 having 1!=count(lastname) order by count(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(lastname) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(lastname) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(lastname) having 1!=count(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(lastname) having 1!=count(lastname) order by count(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 1+count(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1+count(lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

select MAX(lastname) from names group by MAX(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(lastname) from names group by MAX(lastname) having 'Burn'!=MAX(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(lastname) from names group by MAX(lastname) having 'Burn'!=MAX(lastname) order by MAX(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(lastname) from names group by MAX(lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(lastname) from names group by MAX(lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(lastname) from names group by 1 having 'Burn'!=MAX(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(lastname) from names group by 1 having 'Burn'!=MAX(lastname) order by MAX(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(lastname) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MAX(lastname) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(lastname) having 'Burn'!=MAX(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(lastname) having 'Burn'!=MAX(lastname) order by MAX(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MAX(lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 'test'||MAX(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 'test'||MAX(lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

select MIN(lastname) from names group by MIN(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(lastname) from names group by MIN(lastname) having 'Burn'!=MIN(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(lastname) from names group by MIN(lastname) having 'Burn'!=MIN(lastname) order by MIN(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(lastname) from names group by MIN(lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(lastname) from names group by MIN(lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(lastname) from names group by 1 having 'Burn'!=MIN(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(lastname) from names group by 1 having 'Burn'!=MIN(lastname) order by MIN(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(lastname) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select MIN(lastname) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(lastname) having 'Burn'!=MIN(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(lastname) having 'Burn'!=MIN(lastname) order by MIN(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(lastname) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by MIN(lastname) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 'test'||MIN(lastname); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 'test'||MIN(lastname) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

select SUM(id) from names;
select SUM(id) from names group by SUM(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(id) from names group by SUM(id) having 1!=SUM(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(id) from names group by SUM(id) having 1!=SUM(id) order by SUM(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(id) from names group by SUM(id) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(id) from names group by SUM(id) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(id) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(id) from names group by 1 having 1!=SUM(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(id) from names group by 1 having 1!=SUM(id) order by SUM(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(id) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select SUM(id) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(id) having 1!=SUM(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(id) having 1!=SUM(id) order by SUM(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(id) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by SUM(id) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 1+SUM(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1+SUM(id) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

select AVG(id) from names group by AVG(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(id) from names group by AVG(id) having 1!=AVG(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(id) from names group by AVG(id) having 1!=AVG(id) order by AVG(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(id) from names group by AVG(id) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(id) from names group by AVG(id) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(id) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(id) from names group by 1 having 1!=AVG(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(id) from names group by 1 having 1!=AVG(id) order by AVG(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(id) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select AVG(id) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(id) having 1!=AVG(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(id) having 1!=AVG(id) order by AVG(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(id) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by AVG(id) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by 1+AVG(id); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1+AVG(id) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

-- The following queries which use subqueries in ARRAY should work once #745 and #522 are implemented
SELECT ARRAY(select firstname from names) FROM names GROUP BY ARRAY(select firstname from names); -- Octo error saying INVALID USAGE
SELECT ARRAY(select firstname from names) FROM names GROUP BY ARRAY(select firstname from names) HAVING ARRAY(select firstname from names)!='COOL'; -- Octo error saying INVALID USAGE -- Postgres error saying Array in having is malformed
SELECT ARRAY(select firstname from names) FROM names GROUP BY ARRAY(select firstname from names) HAVING ARRAY(select firstname from names)!=lastname; -- Octo error saying INVALID UAGE -- Postgres error operator does not exist: character varying[] <> character varying
SELECT ARRAY(select firstname from names) FROM names GROUP BY ARRAY(select firstname from names) HAVING ARRAY(select firstname from names)=ARRAY(select firstname from names); -- Octo error invalid usage
SELECT ARRAY(select firstname from names) FROM names GROUP BY ARRAY(select firstname from names) HAVING ARRAY(select firstname from names)=ARRAY(select lastname from names); -- Octo error invalid usage
SELECT ARRAY(select firstname from names) FROM names GROUP BY ARRAY(select firstname from names) HAVING ARRAY(select firstname from names)!=ARRAY(select lastname from names) ORDER BY ARRAY(select firstname from names); -- Octo error invalid usage
SELECT ARRAY(select firstname from names) FROM names GROUP BY ARRAY(select firstname from names) HAVING ARRAY(select firstname from names)!=ARRAY(select lastname from names) ORDER BY lastname; -- ERROR as lastname not in GROUP BY
SELECT ARRAY(select firstname from names) FROM names GROUP BY ARRAY(select firstname from names) HAVING lastname!='COOL'; -- Octo error invalid usage -- Postgres ERROR as lastname not in GROUP BY
SELECT lastname FROM names GROUP BY ARRAY(select firstname from names); -- ERROR as lastname not in GROUP BY
select ARRAY(select firstname from names) from names group by lastname having ARRAY(select lastname from names)=lastname order by lastname; -- Resulting from #795 -- Postgres error saying operator does not exist: character varying[] = character varyin
select ARRAY(select firstname from names) from names group by ARRAY(select firstname from names),lastname having ARRAY(select lastname from names)=lastname order by lastname; -- Octo invalid usage-- Postgres error saying operator does not exist: character varying[] = character varyin
SELECT ARRAY(select firstname from names) FROM names GROUP BY 1; -- Octo error subqueries are not supported in group by
SELECT ARRAY(select firstname from names) FROM names GROUP BY 1 HAVING ARRAY(select firstname from names)!=ARRAY(select lastname from names); -- Octo error subqueries are not supported in group by
SELECT ARRAY(select firstname from names) FROM names GROUP BY 1 ORDER BY ARRAY(select firstname from names); -- Octo error subqueries are not supported in group by
SELECT ARRAY(select firstname from names) FROM names GROUP BY 1 HAVING ARRAY(select firstname from names)=ARRAY(select firstname from names) ORDER BY ARRAY(select firstname from names); -- Octo error subqueries are not supported in group by
SELECT ARRAY(select firstname from names) FROM names GROUP BY 1 HAVING ARRAY(select firstname from names)!=ARRAY(select lastname from names) ORDER BY 1; -- Octo error subqueries are not supported in group by
SELECT ARRAY(select firstname from names) FROM names GROUP BY 2; -- Location error
SELECT ARRAY(select firstname from names) FROM names GROUP BY 1 HAVING ARRAY(select firstname from names)!=ARRAY(select firstname from names) ORDER BY 2; -- location error
SELECT ARRAY(select firstname from names) FROM names GROUP BY 1 ORDER BY 2; -- ERROR ORDER BY position is wrong
SELECT ARRAY(select firstname from names) FROM names GROUP BY 2 ORDER BY 2; -- ERROR ORDER BY position is wrong
SELECT 1 FROM names GROUP BY ARRAY(select firstname from names); -- Octo invalid usage
SELECT 1 FROM names GROUP BY ARRAY(select firstname from names) HAVING ARRAY(select firstname from names)!=ARRAY(select lastname from names); -- Octo invalid usage
SELECT 1 FROM names GROUP BY ARRAY(select firstname from names) ORDER BY ARRAY(select firstname from names)!=ARRAY(select lastname from names); -- Octo invalid usage
SELECT 1 FROM names GROUP BY ARRAY(select firstname from names) ORDER BY ARRAY(select firstname from names); -- Octo invalid usage
SELECT 1 FROM names GROUP BY ARRAY(select firstname from names) HAVING ARRAY(select firstname from names)!=ARRAY(select lastname from names); -- Octo invalid usage
SELECT 1 FROM names GROUP BY ARRAY(select firstname from names) HAVING ARRAY(select firstname from names)!=ARRAY(select lastname from names) ORDER BY 1; -- Octo invalid usage

-- Subquery
-- All of the usages in GroupBy should issue an unsupported error
-- TGB20.sql has more richer variation of usecases
-- The following checks mostly the syntax of a basic subquery usage with groupby
SELECT (select firstname from names limit 1) FROM names GROUP BY (select firstname from names limit 1); -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY (select firstname from names limit 1) HAVING (select firstname from names limit 1)!='COOL'; -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY (select firstname from names limit 1) HAVING (select firstname from names limit 1)!=lastname; -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY (select firstname from names limit 1) HAVING (select firstname from names limit 1)=(select firstname from names limit 1); -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY (select firstname from names limit 1) HAVING (select firstname from names limit 1)=(select lastname from names limit 1); -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY (select firstname from names limit 1) HAVING (select firstname from names limit 1)!=(select lastname from names limit 1) ORDER BY (select firstname from names limit 1); -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY (select firstname from names limit 1) HAVING (select firstname from names limit 1)!=(select lastname from names limit 1) ORDER BY lastname; -- Octo invalid usage error -- ERROR as lastname not in GROUP BY
SELECT (select firstname from names limit 1) FROM names GROUP BY (select firstname from names limit 1) HAVING lastname!='COOL'; -- Octo invalid usage error -- ERROR as lastname not in GROUP BY
SELECT lastname FROM names GROUP BY (select firstname from names limit 1); -- Octo invalid usage error -- ERROR as lastname not in GROUP BY
select (select firstname from names limit 1) from names group by (select firstname from names limit 1),lastname having (select lastname from names limit 1)=lastname order by lastname; -- Octo error invalid usag
e
SELECT (select firstname from names limit 1) FROM names GROUP BY 1; -- Octo invalid usage
SELECT (select firstname from names limit 1) FROM names GROUP BY 1 HAVING (select firstname from names limit 1)!=(select lastname from names limit 1); -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY 1 ORDER BY (select firstname from names limit 1); -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY 1 HAVING (select firstname from names limit 1)=(select firstname from names limit 1) ORDER BY (select firstname from names limit 1); -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY 1 HAVING (select firstname from names limit 1)!=(select lastname from names limit 1) ORDER BY 1; -- Octo invalid usage error
SELECT (select firstname from names limit 1) FROM names GROUP BY 2; -- Location error
SELECT (select firstname from names limit 1) FROM names GROUP BY 1 HAVING (select firstname from names limit 1)!=(select firstname from names limit 1) ORDER BY 2; -- location error
SELECT (select firstname from names limit 1) FROM names GROUP BY 1 ORDER BY 2; -- ERROR ORDER BY position is wrong
SELECT (select firstname from names limit 1) FROM names GROUP BY 2 ORDER BY 2; -- ERROR ORDER BY position is wrong
SELECT 1 FROM names GROUP BY (select firstname from names limit 1); -- Octo invalid usage error
SELECT 1 FROM names GROUP BY (select firstname from names limit 1) HAVING (select firstname from names limit 1)!=(select lastname from names limit 1); -- Octo invalid usage error
SELECT 1 FROM names GROUP BY (select firstname from names limit 1) ORDER BY (select firstname from names limit 1)!=(select lastname from names limit 1); -- Octo invalid usage error
SELECT 1 FROM names GROUP BY (select firstname from names limit 1) ORDER BY (select firstname from names limit 1); -- Octo invalid usage error
SELECT 1 FROM names GROUP BY (select firstname from names limit 1) HAVING (select firstname from names limit 1)!=(select lastname from names limit 1); -- Octo invalid usage error
SELECT 1 FROM names GROUP BY (select firstname from names limit 1) HAVING (select firstname from names limit 1)!=(select lastname from names limit 1) ORDER BY 1; -- Octo invalid usage error

-- Exists operation
SELECT exists (select firstname from names limit 1) FROM names GROUP BY exists (select firstname from names limit 1); -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY exists (select firstname from names limit 1) HAVING exists (select firstname from names limit 1); -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY exists (select firstname from names limit 1) HAVING exists (select firstname from names limit 1) order by exists (select firstname from names limit 1); -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY exists (select firstname from names limit 1) HAVING exists (select firstname from names limit 1)=TRUE; -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY exists (select firstname from names limit 1) HAVING exists (select lastname from names limit 1); -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY exists (select firstname from names limit 1) HAVING exists (select lastname from names limit 1) ORDER BY exists (select firstname from names limit 1); -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY exists (select firstname from names limit 1) HAVING exists (select lastname from names limit 1) ORDER BY lastname; -- Octo subqueries not allowed error -- ERROR as lastname not in GROUP BY
SELECT exists (select firstname from names limit 1) FROM names GROUP BY exists (select firstname from names limit 1) HAVING lastname!='Cool'; -- Octo subqueries not allowed error -- ERROR as lastname not in GROUP BY
SELECT lastname FROM names GROUP BY exists (select firstname from names limit 1); -- Octo subqueries not allowed error -- ERROR as lastname not in GROUP BY
select exists (select firstname from names limit 1) from names group by exists (select firstname from names limit 1),lastname having exists (select lastname from names limit 1)=lastname order by lastname; -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY 1; -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY 1 HAVING exists (select lastname from names limit 1); -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY 1 ORDER BY exists (select firstname from names limit 1); -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY 1 HAVING exists (select firstname from names limit 1) ORDER BY exists (select firstname from names limit 1); -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY 1 HAVING exists (select lastname from names limit 1) ORDER BY 1; -- Octo subqueries not allowed error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY 2; -- Location error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY 1 HAVING exists (select firstname from names limit 1)!=exists (select firstname from names limit 1) ORDER BY 2; -- Octo subqueries not allowed error -- location error
SELECT exists (select firstname from names limit 1) FROM names GROUP BY 1 ORDER BY 2; -- Octo subqueries not allowed error -- ERROR ORDER BY position is wrong
SELECT exists (select firstname from names limit 1) FROM names GROUP BY 2 ORDER BY 2; -- Octo subqueries not allowed error -- ERROR ORDER BY position is wrong
SELECT 1 FROM names GROUP BY exists (select firstname from names limit 1); -- Octo subqueries not allowed error
SELECT 1 FROM names GROUP BY exists (select firstname from names limit 1) HAVING exists (select lastname from names limit 1); -- Octo subqueries not allowed error
SELECT 1 FROM names GROUP BY exists (select firstname from names limit 1) ORDER BY exists (select lastname from names limit 1); -- Octo subqueries not allowed error
SELECT 1 FROM names GROUP BY exists (select firstname from names limit 1) ORDER BY exists (select firstname from names limit 1); -- Octo subqueries not allowed error
SELECT 1 FROM names GROUP BY exists (select firstname from names limit 1) HAVING exists (select lastname from names limit 1); -- Octo subqueries not allowed error
SELECT 1 FROM names GROUP BY exists (select firstname from names limit 1) HAVING exists (select lastname from names limit 1) ORDER BY 1; -- Octo subqueries not allowed error
select exists (select firstname from names limit 1) from names group by lastname having exists (select lastname from names limit 1)=lastname order by lastname; -- Type missmatch

-- quantified_comparison_predicate
-- The following checks mostly the syntax of a basic subquery usage with groupby
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING lastname!=ANY(select firstname from names limit 1); -- octo subquery error -- Postgres error lastname not in GroupBy
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING 'Zero'!=ANY(select firstname from names limit 1)='Zero'!=ANY(select lastname from names limit 1); -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING 'Zero'!=ANY(select firstname from names limit 1) ORDER BY 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING 'Zero'!=ANY(select firstname from names limit 1) ORDER BY lastname; -- octo subquery error -- ERROR as lastname not in GROUP BY
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING lastname!='COOL'; -- octo subquery error -- ERROR as lastname not in GROUP BY
SELECT lastname FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error -- ERROR as lastname not in GROUP BY
select 'Zero'!=ANY(select firstname from names limit 1) from names group by 'Zero'!=ANY(select firstname from names limit 1),lastname having lastname!=ANY(select lastname from names limit 1) order by lastname; -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 1; -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 1 HAVING 'Zero'!=ANY(select firstname from names limit 1) from names limit 1); -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 1 ORDER BY 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 1 HAVING 'Zero'!=ANY(select firstname from names limit 1) ORDER BY 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 1 HAVING 'Zero'!=ANY(select firstname from names limit 1) ORDER BY 1; -- octo subquery error
SELECT lastname FROM names GROUP BY 1 HAVING 'Zero'!=ANY(select firstname from names limit 1) ORDER BY 1; -- octo subquery error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 2; -- Location error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 1 HAVING 'Zero'!=ANY(select firstname from names limit 1) ORDER BY 2;  -- octo subquery error-- location error
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 1 ORDER BY 2;  -- octo subquery error-- ERROR ORDER BY position is wrong
SELECT 'Zero'!=ANY(select firstname from names limit 1) FROM names GROUP BY 2 ORDER BY 2; -- ERROR ORDER BY position is wrong
SELECT 1 FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 1 FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 1 FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) ORDER BY 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 1 FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) ORDER BY 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 1 FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING 'Zero'!=ANY(select firstname from names limit 1); -- octo subquery error
SELECT 1 FROM names GROUP BY 'Zero'!=ANY(select firstname from names limit 1) HAVING 'Zero'!=ANY(select firstname from names limit 1) ORDER BY 1; -- octo subquery error

-- Between operation
-- between_predicate -> x between comparison_predicate AND comparison_predicate
--                   -> x not between comparison_predicate and comparison_predicate
SELECT id between 0 and 4 FROM names GROUP BY id between 0 and 4 HAVING firstname!='COOL'; -- Postgres ERROR saying firstname not in GroupBy
select id between 0 and 4 from names group by id between 0 and 4 having id!=1; -- Postgre ERRor id not in GroupBy
select id between 0 and 4 from names group by id between 1 and 4; -- Postgres error id not in Groupby
select id between 0 and 4 from names group by id between 0 and 4 having id between 1 and 4; --Postgres error id of having not in GroupBy
select id between 0 and 4 from names group by id between 0 and 4 having id between 0 and 4 order by id between 1 and 4; -- Postgres error id of order by not in Groupby
select id between id and id+1 from names group by id between id and id+2; -- Postgres error id not in Groupby
select id between id and id+1 from names group by id between id and id+1 having id between id and id+2; -- Postgres error id not in GroupBy
select id between id and id+1 from names group by id between id and id+1 order by id between id and id+2; -- Postgres error id not in Groupby
SELECT id between 0 and 4 FROM names GROUP BY 1 HAVING firstname!='COOL'; -- Postgres ERROR saying firstname not in GroupBy
select id between 0 and 4 from names group by 1 having id!=1; -- Postgre ERRor id not in GroupBy
select id between 0 and 4 from names group by 1 having id between 1 and 4; --Postgres error id of having not in GroupBy
select id between 0 and 4 from names group by 1 having id between 0 and 4 order by id between 1 and 4; -- Postgres error id of order by not in Groupby
select id between id and id+1 from names group by 1 having id between id and id+2; -- Postgres error id not in GroupBy
select id between id and id+1 from names group by 1 order by id between id and id+2; -- Postgres error id not in Groupby
SELECT id between 0 and 4 FROM names GROUP BY 2; -- location error
SELECT id between 0 and 4 FROM names GROUP BY 1 ORDER BY 2; -- location error
SELECT id between 0 and 4 FROM names GROUP BY 2 ORDER BY 2; -- location error
SELECT id between 0 and 4 FROM names GROUP BY lastname; -- Postgres error saying id must be in GROUP BY
SELECT lastname,firstname FROM names GROUP BY id between 0 and 4; -- Postgres error saying lastname and firstname FROM SELECT list must be in groupby
SELECT 1 FROM names GROUP BY id between 0 and 4 HAVING lastname!='cool'; -- Postgres error lastname must appear in GROUP BY
SELECT 1 FROM names GROUP BY id between 0 and 4 ORDER BY lastname; -- Postgres error lastname must appear in GROUP BY

-- NOT between operation
SELECT id not between 0 and 4 FROM names GROUP BY id not between 0 and 4 HAVING firstname!='COOL'; -- Postgres ERROR saying firstname not in GroupBy
select id not between 0 and 4 from names group by id not between 0 and 4 having id!=1; -- Postgre ERRor id not in GroupBy
select id not between 0 and 4 from names group by id not between 1 and 4; -- Postgres error id not in Groupby
select id not between 0 and 4 from names group by id not between 0 and 4 having id not between 1 and 4; --Postgres error id of having not in GroupBy
select id not between 0 and 4 from names group by id not between 0 and 4 having id not between 0 and 4 order by id not between 1 and 4; -- Postgres error id of order by not in Groupby
select id not between id and id+1 from names group by id not between id and id+2; -- Postgres error id not in Groupby
select id not between id and id+1 from names group by id not between id and id+1 having id not between id and id+2; -- Postgres error id not in GroupBy
select id not between id and id+1 from names group by id not between id and id+1 order by id not between id and id+2; -- Postgres error id not in Groupby
SELECT id not between 0 and 4 FROM names GROUP BY 1 HAVING firstname!='COOL'; -- Postgres ERROR saying firstname not in GroupBy
select id not between 0 and 4 from names group by 1 having id!=1; -- Postgre ERRor id not in GroupBy
select id not between 0 and 4 from names group by 1 having id not between 1 and 4; --Postgres error id of having not in GroupBy
select id not between 0 and 4 from names group by 1 having id not between 0 and 4 order by id not between 1 and 4; -- Postgres error id of order by not in Groupby
select id not between id and id+1 from names group by 1 having id not between id and id+2; -- Postgres error id not in GroupBy
select id not between id and id+1 from names group by 1 order by id not between id and id+2; -- Postgres error id not in Groupby
SELECT id not between 0 and 4 FROM names GROUP BY 2; -- location error
SELECT id not between 0 and 4 FROM names GROUP BY 1 ORDER BY 2; -- location error
SELECT id not between 0 and 4 FROM names GROUP BY 2 ORDER BY 2; -- location error
SELECT id not between 0 and 4 FROM names GROUP BY lastname; -- Postgres error saying id must be in GROUP BY
SELECT lastname,firstname FROM names GROUP BY id not between 0 and 4; -- Postgres error saying lastname and firstname FROM SELECT list must be in groupby
SELECT 1 FROM names GROUP BY id not between 0 and 4 HAVING lastname!='cool'; -- Postgres error lastname must appear in GROUP BY
SELECT 1 FROM names GROUP BY id not between 0 and 4 ORDER BY lastname; -- Postgres error lastname must appear in GROUP BY
select id not between 0 and 4 from names group by id between 0 and 4; -- Postgres error id not in GroupBy
select id not between 0 and 4 from names group by id not between 0 and 4 order by id between 0 and 4; -- Postgres error id in order by not in Groupby
select id not between 0 and 4 from names group by id not between 0 and 4 having id between 0 and 4; -- Postgres error id in having not in groupby

-- IN operation
select lastname in ('Cool','Killer',NULL) from names group by firstname; -- Postgres error lastname not in groupby
select lastname in ('Cool','Killer',NULL) from names group by lastname in ('Cool',NULL,'Killer'); -- Postgres error lastname not in groupby
select lastname in ('Cool','Killer',NULL) from names group by lastname in ('Cool','Killer',NULL) having lastname in ('Cool',NULL, 'Killer'); -- postgres error lastname in having not in groupby
select lastname in ('Cool','Killer',NULL) from names group by lastname in ('Cool','Killer',NULL) order by lastname in ('Cool',NULL,'Killer'); -- postgres error lastname in order by not in groupby
select lastname in ('Cool','Killer',NULL) from names group by lastname in ('Cool','Killer',NULL),firstname order by lastname in ('Cool',NULL,'Killer'); -- postgres error lastname in order by not in groupby
select lastname in (firstname,'Killer',NULL) from names group by firstname; -- Postgres error lastname not in groupby
select lastname in (firstname,'Killer',NULL) from names group by lastname; -- Postgres error firstnam not in gropu by
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,NULL,'Killer'); -- Postgres error lastname not in groupby
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,'Killer',NULL) having lastname in (firstname,NULL, 'Killer'); -- postgres error lastname in having not in groupby
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,'Killer',NULL) order by lastname in (firstname,NULL,'Killer'); -- postgres error lastname in order by not in groupby
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,'Killer',NULL),firstname order by lastname in (firstname,NULL,'Killer'); -- postgres error lastname in order by not in groupby
select lastname in ('Cool','Killer',NULL) from names group by 1 having lastname in ('Cool',NULL, 'Killer'); -- postgres error lastname in having not in groupby
select lastname in ('Cool','Killer',NULL) from names group by 1 order by lastname in ('Cool',NULL,'Killer'); -- postgres error lastname in order by not in groupby
select lastname in ('Cool','Killer',NULL) from names group by 1,firstname order by lastname in ('Cool',NULL,'Killer'); -- postgres error lastname in order by not in groupby
select lastname in (firstname,'Killer',NULL) from names group by 1 having lastname in (firstname,NULL, 'Killer'); -- postgres error lastname in having not in groupby
select lastname in (firstname,'Killer',NULL) from names group by 1 having lastname in (firstname, NULL, 'Killer') order by 1; -- Postgres error lastname in having not in group by
select lastname in (firstname,'Killer',NULL) from names group by 1 order by lastname in (firstname,NULL,'Killer'); -- postgres error lastname in order by not in groupby
select lastname in (firstname,'Killer',NULL) from names group by 1,firstname order by lastname in (firstname,NULL,'Killer'); -- postgres error lastname in order by not in groupby
select 1 from names group by lastname in ('Cool','Killer',NULL) having lastname in ('Cool',NULL, 'Killer'); -- postgres error lastname in having not in groupby
select 1 from names group by lastname in ('Cool','Killer',NULL) order by lastname in ('Cool',NULL,'Killer'); -- postgres error lastname in order by not in groupby
select 1 from names group by lastname in ('Cool','Killer',NULL),firstname order by lastname in ('Cool',NULL,'Killer'); -- postgres error lastname in order by not in groupby
select 1 from names group by lastname in (firstname,'Killer',NULL) having lastname in (firstname,NULL, 'Killer'); -- postgres error lastname in having not in groupby
select 1 from names group by lastname in (firstname,'Killer',NULL) having lastname in (firstname, NULL, 'Killer') order by lastname in (firstname,'Killer',NULL); -- Postgres error lastname in having not in group by
select 1 from names group by lastname in (firstname,'Killer',NULL) order by lastname in (firstname,NULL,'Killer'); -- postgres error lastname in order by not in groupby
select 1 from names group by lastname in (firstname,'Killer',NULL),firstname order by lastname in (firstname,NULL,'Killer'); -- postgres error lastname in order by not in groupby
select firstname from names group by lastname in (firstname,'Killer',NULL); -- Postgres error firstname not in group by
select lastname in (firstname,'Killer',NULL) from names group by lastname in (firstname,'Killer',NULL),lastname order by lastname in (firstname,NULL,'Killer'); -- postgres firstname must be in group by
select 1 from names group by lastname in (firstname,'Killer',NULL),lastname order by lastname in (firstname,NULL,'Killer'); -- Postgres error firstname must be in group by

-- INoperation with subquery
-- All of the queries with in subquery usage in group by should result in error
select lastname in (select 'Cool') from names group by firstname; -- Postgres error lastname not in groupby
select lastname in (select 'Cool') from names group by lastname in (select 'Killer'); -- Postgres error lastname not in groupby
select lastname in (select 'Cool') from names group by lastname in (select 'Cool') having lastname in (select 'Killer'); -- postgres error lastname in having not in groupby
select lastname in (select 'Cool') from names group by lastname in (select 'Cool') order by lastname in (select 'Killer'); -- postgres error lastname in order by not in groupby
select lastname in (select 'Cool') from names group by lastname in (select 'Cool'),firstname order by lastname in (select 'Killer'); -- postgres error lastname in order by not in groupby
select lastname in (select lastname from names) from names group by firstname; -- Postgres error lastname not in groupby
select lastname in (select lastname from names) from names group by lastname in (select firstname from names); -- Postgres error lastname not in groupby
select lastname in (select lastname from names) from names group by lastname in (select lastname from names) having lastname in (select firstname from names); -- postgres error lastname in having not in groupby
select lastname in (select lastname from names) from names group by lastname in (select lastname from names) order by lastname in (select firstname from names); -- postgres error lastname in order by not in groupby
select lastname in (select lastname from names) from names group by lastname in (select lastname from names),firstname order by lastname in (select firstname from names); -- postgres error lastname in order by not in groupby
select lastname in (select 'Cool') from names group by 1 having lastname in (select 'Killer'); -- postgres error lastname in having not in groupby
select lastname in (select 'Cool') from names group by 1 order by lastname in (select 'Killer'); -- postgres error lastname in order by not in groupby
select lastname in (select 'Cool') from names group by 1,firstname order by lastname in (select 'Killer'); -- postgres error lastname in order by not in groupby
select lastname in (select lastname from names) from names group by 1 having lastname in (select firstname from names); -- postgres error lastname in having not in groupby
select lastname in (select lastname from names) from names group by 1 having lastname in (select firstname from names) order by 1; -- Postgres error lastname in having not in group by
select lastname in (select lastname from names) from names group by 1 order by lastname in (select firstname from names); -- postgres error lastname in order by not in groupby
select lastname in (select lastname from names) from names group by 1,firstname order by lastname in (select firstname from names); -- postgres error lastname in order by not in groupby
select 1 from names group by lastname in (select 'Cool') having lastname in (select 'Killer'); -- postgres error lastname in having not in groupby
select 1 from names group by lastname in (select 'Cool') order by lastname in (select 'Killer'); -- postgres error lastname in order by not in groupby
select 1 from names group by lastname in (select 'Cool'),firstname order by lastname in (select 'Killer'); -- postgres error lastname in order by not in groupby
select 1 from names group by lastname in (select lastname from names) having lastname in (select firstname from names); -- postgres error lastname in having not in groupby
select 1 from names group by lastname in (select lastname from names) having lastname in (select firstname from names) order by lastname in (select lastname from names); -- Postgres error lastname in having not in group by
select 1 from names group by lastname in (select lastname from names) order by lastname in (select firstname from names); -- postgres error lastname in order by not in groupby
select 1 from names group by lastname in (select lastname from names),firstname order by lastname in (select firstname from names); -- postgres error lastname in order by not in groupby
select firstname from names group by lastname in (select lastname from names); -- Postgres error firstname not in group by
-- All the below should error as they are all having sub-queries in group by and this usage is not yet supported
select lastname in (select 'Cool') from names group by lastname in (select 'Cool');
select lastname in (select 'Cool') from names group by lastname in (select 'Cool') having lastname in (select 'Cool');
select lastname in (select 'Cool') from names group by lastname in (select 'Cool') order by lastname in (select 'Cool');
select lastname in (select 'Cool') from names group by lastname in (select 'Cool'),lastname order by lastname in (select 'Killer');
select lastname in (select lastname from names) from names group by lastname in (select lastname from names);
select lastname in (select lastname from names) from names group by lastname in (select lastname from names) having lastname in (select lastname from names);
select lastname in (select lastname from names) from names group by lastname in (select lastname from names) order by lastname in (select lastname from names);
select lastname in (select lastname from names) from names group by lastname in (select lastname from names),lastname order by lastname in (select firstname from names);
select lastname in (select 'Cool') from names group by 1;
select lastname in (select 'Cool') from names group by 1 having lastname in (select 'Cool');
select lastname in (select 'Cool') from names group by 1 order by lastname in (select 'Cool');
select lastname in (select 'Cool') from names group by 1,lastname order by lastname in (select 'Killer');
select lastname in (select lastname from names) from names group by 1;
select lastname in (select lastname from names) from names group by lastname in (select lastname from names) having lastname in (select lastname from names);
select lastname in (select lastname from names) from names group by 1 order by lastname in (select lastname from names);
select lastname in (select lastname from names) from names group by 1 order by 1;
select lastname in (select lastname from names) from names group by 1 having lastname in (select lastname from names) order by 1;
select lastname in (select lastname from names) from names group by 1,lastname order by lastname in (select firstname from names);
select 1 from names group by lastname in (select 'Cool') having lastname in (select 'Cool');
select 1 from names group by lastname in (select 'Cool') order by lastname in (select 'Cool');
select 1 from names group by lastname in (select 'Cool'),lastname order by lastname in (select 'Killer');
select 1 from names group by lastname in (select 'Cool');
select 1 from names group by lastname in (select lastname from names) having lastname in (select lastname from names);
select 1 from names group by lastname in (select lastname from names) order by lastname in (select lastname from names);
select 1 from names group by lastname in (select lastname from names) having lastname in (select lastname from names) order by lastname in (select lastname from names);
select 1 from names group by lastname in (select lastname from names),lastname order by lastname in (select firstname from names);
select firstname from names group by lastname in (select lastname from names),firstname;

-- Following double quote queries have valid use cases as well but are not co-herent with postgres to test using cross check interface
-- so adding them here.
SELECT 'test' FROM names GROUP BY 'test';
SELECT 'test' FROM names GROUP BY 'test' HAVING 'test'!='hello';
SELECT 'test' FROM names GROUP BY 'test' ORDER BY 'test';
SELECT 'test' FROM names GROUP BY 1;
SELECT 'test' FROM names GROUP BY 1 ORDER BY 'test';
SELECT 'test' FROM names GROUP BY 1 HAVING 'test'!='hello' ORDER BY 'test';
SELECT 'test' FROM names GROUP BY 1 HAVING 'test'!='hello';
SELECT 'test' FROM names GROUP BY firstname HAVING 'test'!='hello' ORDER BY 'test';
SELECT 'test' FROM names GROUP BY firstname HAVING 'test'!='hello' ORDER BY 1;
SELECT 'test' FROM names GROUP BY 1 HAVING 'test'!='hello' ORDER BY firstname; -- errors out saying firstname in ORDER BY must be in GROUP BY

SELECT CAST('12' AS INTEGER) FROM names GROUP BY 1 HAVING CAST('12' AS INTEGER)=12 ORDER BY firstname; -- errors out saying firstname in ORDER BY must be in GROUP BY
SELECT CAST(id AS NUMERIC) FROM names GROUP BY firstname HAVING CAST(id AS NUMERIC)=3.0 ORDER BY 1; -- Octo expression error -- Postgres Error as id not in group by

-- With CAST_SPECIFICATION
select id::NUMERIC from names group by id::NUMERIC having 1=id; -- Postgres error id in having not in GroupBy
select id::NUMERIC from names group by id::NUMERIC having 1=id::NUMERIC order by id; -- Postgres error id in order by not in GroupBy
select 1 from names group by id::NUMERIC having 1=id; -- Postgres error id in having not in GroupBy
select id::NUMERIC from names group by 2; -- Postgres error case incorrect location
select 1 from names group by id::NUMERIC having 1=id::NUMERIC order by id; -- Postgres error id in order by not in GroupBy
select id from names group by id::NUMERIC; -- Postgres error saying id not in group by
select id::NUMERIC from names group by id::INTEGER having 1=id::NUMERIC; -- Postgres allows this but Octo generates an error, #821 is created to track this query

-- With CAST_EXPRESSION usage
select CAST(id AS NUMERIC) from names group by CAST(id AS NUMERIC) having 1=id; -- Postgres error id in having not in GroupBy
select CAST(id AS NUMERIC) from names group by CAST(id AS NUMERIC) having 1=CAST(id AS NUMERIC) order by id; -- Postgres error id in order by not in GroupBy
select 1 from names group by CAST(id AS NUMERIC) having 1=id; -- Postgres error id in having not in GroupBy
select 1 from names group by CAST(id AS NUMERIC) having 1=CAST(id AS NUMERIC) order by id; -- Postgres error id in order by not in GroupBy
select CAST(id AS NUMERIC) from names group by 2; -- Postgres error case incorrect location

-- COUNT(*) usage
select count(*) from names group by count(*); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(*) from names group by count(*) having 1!=count(*); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(*) from names group by count(*) having 1!=count(*) order by count(*); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(*) from names group by count(*) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(*) from names group by count(*) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(*) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(*) from names group by 1 having 1!=count(*); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(*) from names group by 1 having 1!=count(*) order by count(*); -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(*) from names group by 1 having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(*) from names group by 1 having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(*); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(*) having 1!=count(*); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(*) having 1!=count(*) order by count(*); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(*) having 1!=1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1 from names group by count(*) having 1!=1 order by 1+1; -- Postgres error saying aggregate functions are not allowed in GroupBy
select count(1+*); -- error case which is supposed to error out at the parser itself
select 1 from names group by 1+count(*); -- Postgres error saying aggregate functions are not allowed in GroupBy
select 1+count(*) from names group by 1; -- Postgres error saying aggregate functions are not allowed in GroupBy

-- ASTERISK
select * from names group by 1;
SELECT * FROM names GROUP BY *; -- Postgres generates a syntax error
SELECT 1 FROM names GROUP BY *; -- Postgres generates a syntax error
SELECT * FROM names ORDER BY *; -- Postgres generates a syntax error at GROUP BY *
SELECT * FROM names GROUP BY * ORDER BY 1;
SELECT * FROM names GROUP BY 1 ORDER BY id;
SELECT * FROM names GROUP BY 1 ORDER BY firstname;
SELECT * FROM names GROUP BY 1 ORDER BY lastname;
SELECT * FROM names GROUP BY 1 ORDER BY 1;
SELECT * FROM names GROUP BY 1 HAVING * is not null; -- Postgres syntax error at HAVING clause *
SELECT 1 FROM names GROUP BY 1 HAVING * is not null; -- Postgres syntax error at HAVING clause *
SELECT * FROM names GROUP BY 1 HAVING lastname is not null;
SELECT * FROM names GROUP BY 1 HAVING firstname is not null;
SELECT * FROM names GROUP BY 1 HAVING id is not null;
SELECT * FROM names GROUP BY 1 HAVING lastname is not null ORDER BY *; -- Postgres syntax error at HAVING clause *
SELECT * FROM names GROUP BY 1 HAVING lastname is not null ORDER BY 1;
SELECT * FROM names GROUP BY 1 HAVING lastname is not null ORDER BY lastname;
SELECT * FROM names GROUP BY 1 HAVING lastname is not null ORDER BY firstname;
SELECT 1 FROM names GROUP BY 1 HAVING lastname is not null ORDER BY lastname; -- Postgres error saying lastname not in GROUP BY

-- TABLE_ASTERISK
SELECT n1.* FROM names n1 GROUP BY n1.*; -- Postgres generates an error saying n1.id in SELECT list is not included in GROUP BY
SELECT n1.* FROM names n1 GROUP BY 1; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY 1 ORDER BY n1.*; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY n1.* ORDER BY n1.*; -- Postgres generates an error saying n1.id in SELECT list is not included in GROUP BY
SELECT n1.* FROM names n1 GROUP BY 1 HAVING n1.* > 1; -- Octo Expression error -- ERROR Postgres no operator matches HAVING clause usage
SELECT n1.* FROM names n1 GROUP BY 1 HAVING n1.* is null; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY 1 HAVING n1.* is not null; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY 1 HAVING n1.* is not null ORDER BY n1.*; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY 1 HAVING n1.* is not null ORDER BY 1; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY 1 HAVING n1.firstname is null; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY 1 ORDER BY n1.firstname; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY n1.* ORDER BY n1.firstname; -- Postgres ERROR saying n1.id in SELECT list not in GroupBy
SELECT n1.* FROM names n1 GROUP BY 1 HAVING n1.lastname!='Zero' ORDER BY n1.firstname; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY 1 HAVING n1.lastname!='Zero' ORDER BY n1.*; --Octo issues an error cause of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/675 -- Postgres Works fine
SELECT n1.* FROM names n1 GROUP BY n1.* HAVING n1.lastname!='Zero' ORDER BY n1.firstname; -- Postgres ERROR saying n1.id in SELECT list not in GroupBy
SELECT n1.* FROM names n1 GROUP BY n1.* HAVING n1.lastname!='Zero' ORDER BY n1.*; -- Postgres ERROR saying n1.id in SELECT list not in GroupBy

-- misc
-- Similar to the query in #457
select lastname, EXISTS (SELECT alias1.lastName FROM names alias1 GROUP BY 'test'||alias1.lastName) from names GROUP BY 1;
select 'test'||lastname, EXISTS (SELECT alias1.lastname FROM names alias1 GROUP BY 'test'||alias1.lastname) from names GROUP BY 1,lastname;
select lastname, EXISTS (SELECT alias1.lastName FROM names alias1 GROUP BY (select 1) from names GROUP BY 1;
SELECT names.lastName FROM names GROUP BY names.lastName HAVING EXISTS (SELECT alias1.lastName FROM names alias1 GROUP BY 1+(select 1));
-- Ensure GROUP BY validation happen when constants are in GROUP BY
SELECT id from names group by 1+1;

-- A non integer constant used in GROUP BY
select id from names group by 11111111111111111111111111111;


