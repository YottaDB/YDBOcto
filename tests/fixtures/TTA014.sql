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

-- Basic tests
select n1.* < n1.* from names n1;
select 1 from names n1 order by n1.* < n1.*;
select n1.* < n1.* from names n1 group by n1.*;
select 1 from names n1 group by n1.* < n1.*;
select n1.* >= n1.* from names n1; -- Good test to validate NULL condition handling
select n1.* > n1.* from names n1;
select n1.* <= n1.* from names n1;
select n1.* < n1.* from names n1;
select n1.* != n1.* from names n1;
select n1.* = n1.* from names n1;
select id+1 from names n1 order by n1.* < n1.*;

-- Validate comparison between specific literal values
select n1.* < n2.* from (VALUES('test','test')) n1 JOIN (select 'test',NULL) n2 on (1=1);
select n1.* < n2.* from (VALUES('test','test')) n1 JOIN (select 'test','test') n2 on (1=1);
select n1.* < n2.* from (VALUES('test','test')) n1 JOIN (select 'test','testa') n2 on (1=1);
select n1.* < n2.* from (VALUES(NULL,NULL) ,('test','test')) n1 JOIN (select NULL,NULL union select 'test','test') n2 on (1=1);

select n1.* != n2.* from (VALUES('te','st')) n1 JOIN (select 'tes','t') n2 on (n1.column1 IS NOT NULL);
select 1 from (VALUES('te','st')) n1 order BY TRUE in (select n1.* = n2.* from (select 'tes','t')n2) ;
select 1 from (VALUES('te','st')) n1 order BY TRUE in (select n1.* != n2.* from (select 'tes','t')n2) ;
select 1 from (VALUES('te','st')) n1 where FALSE in (select n1.* = n2.* from (select 'tes','t')n2) ;
select 1 from (VALUES('te','st')) n1 where TRUE in (select n1.* = n2.* from (select 'tes','t')n2) ;

-- table.* is a composite type and not a row constructor because the later type treats NULL = non-NULL as NULL where as the former evaluates the expression as false
select n1.* = n2.* from (VALUES ('test') ,(NULL), ('test')) n1, (select 'test') n2;

-- Regular string comparison working
select n1.* < n2.* from (VALUES('test') ,('tes') ,('test')) n1 JOIN (select 'test' union select 'test' union select 'test') n2 on (1=1);
select n1.* <= n2.* from (VALUES('test') ,('tes') ,('test')) n1 JOIN (select 'test' union select 'test' union select 'test') n2 on (1=1);
select n1.* > n2.* from (VALUES('test') ,('testa') ,('test')) n1 JOIN (select 'test' union select 'test' union select 'test') n2 on (1=1);
select n1.* >= n2.* from (VALUES('test') ,('testa') ,('test')) n1 JOIN (select 'test' union select 'test' union select 'test') n2 on (1=1);

-- NULL is considered larger than non-NULL and NULL is considered equal to a NULL
select n1.* < n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select 'test' union select NULL union select 'test') n2 on (1=1);
select n1.* <= n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select 'test' union select NULL union select 'test') n2 on (1=1);
select n1.* > n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select 'test' union select NULL union select 'test') n2 on (1=1);
select n1.* >= n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select 'test' union select NULL union select 'test') n2 on (1=1);

-- Strings having integers are still evaluated as strings
select n1.* = n2.* from (VALUES('3') ,('3.0') ,('3')) n1 JOIN (select '3' union select '3.0' union select '3') n2 on (1=1);

-- Misc
select n1.* > n2.* from (VALUES('32') ,('32') ,('32')) n1 JOIN (select '100' union select '100' union select '100') n2 on (1=1);
select n1.* > n2.* from (VALUES(32) ,(32) ,(32)) n1 JOIN (select 100 union select 100 union select 100) n2 on (1=1);
select n1.* >= n2.* from (VALUES(1,'test1',1) ,(2,'test2',2) ,(1,'test3',3)) n1 JOIN (select 1,'test1',1 union select 2, NULL,2 union select 3,'test3',3) n2 on (1=1);
select n1.* > n2.* from (VALUES(1,'test1',1) ,(2,'test2',2) ,(1,'test3',3)) n1 JOIN (select 1,'test1',1 union select 2, NULL,2 union select 3,'test3',3) n2 on (1=1);
select n1.* > n2.* from (VALUES(1,2,3)) n1 JOIN (select 1,1,1) n2 on (1=1);

-- IS NULL
select n1.* is NULL from (select lastname from names) n1;
select n1.* is NULL from names n1;
select n1.* is NOT NULL from (select lastname from names) n1;
select n1.* is NOT NULL from names n1;

-- IN operation
select n1.* in (n2.*) from (VALUES(1,2,3)) n1 JOIN (select 1,1,1) n2 on (1=1);
select n1.* in (NULL) from (VALUES(1,'1',3)) n1;

-- NULL literal comparison results in NULL output
select n1.* = NULL from (VALUES(1,'1',3)) n1;
select n1.* < NULL from (VALUES(1,'1',3)) n1;
select n1.* < NULL from (VALUES(NULL)) n1;

-- NUMERIC comparison
select n1.* >= n2.* from (VALUES(1,1.0,1) ,(2,1.1,2) ,(1,1.2,3)) n1 JOIN (select 1,1.0,1 union select 2, NULL,2 union select 3,1.2,3) n2 on (1=1);
select n1.* > n2.* from (VALUES(1,1.0,1) ,(2,1.1,2) ,(1,1.2,3)) n1 JOIN (select 1,1.0,1 union select 2, NULL,2 union select 3,1.2,3) n2 on (1=1);
select n1.* < n2.* from (VALUES(1,1.0,1) ,(2,1.1,2) ,(1,1.2,3)) n1 JOIN (select 1,1.0,1 union select 2, NULL,2 union select 3,1.2,3) n2 on (1=1);
select n1.* <= n2.* from (VALUES(1,1.0,1) ,(2,1.1,2) ,(1,1.2,3)) n1 JOIN (select 1,1.0,1 union select 2, NULL,2 union select 3,1.2,3) n2 on (1=1);
select n1.* = n2.* from (VALUES(1,1.0,1) ,(2,1.1,2) ,(1,1.2,3)) n1 JOIN (select 1,1.0,1 union select 2, NULL,2 union select 3,1.2,3) n2 on (1=1);
select n1.* != n2.* from (VALUES(1,1.0,1) ,(2,1.1,2) ,(1,1.2,3)) n1 JOIN (select 1,1.0,1 union select 2, NULL,2 union select 3,1.2,3) n2 on (1=1);
select n1.* = n2.* from (select round(avg(id),0) from names where id = 3) n1, (select round(avg(id),1) from names where id = 3) n2;
select n1.* = n2.* from (select round(avg(id),2) from names where id = 3) n1, (select round(avg(id),1) from names where id = 3) n2;

-- Boolean comparison
select n1.* >= n2.* from (VALUES(1,TRUE,1) ,(2,TRUE,2) ,(1,TRUE,3)) n1 JOIN (select 1,TRUE,1 union select 2, NULL,2 union select 3,FALSE,3) n2 on (1=1);
select n1.* > n2.* from (VALUES(1,TRUE,1) ,(2,TRUE,2) ,(1,TRUE,3)) n1 JOIN (select 1,TRUE,1 union select 2, NULL,2 union select 3,FALSE,3) n2 on (1=1);
select n1.* < n2.* from (VALUES(1,TRUE,1) ,(2,TRUE,2) ,(1,TRUE,3)) n1 JOIN (select 1,TRUE,1 union select 2, NULL,2 union select 3,FALSE,3) n2 on (1=1);
select n1.* <= n2.* from (VALUES(1,TRUE,1) ,(2,TRUE,2) ,(1,TRUE,3)) n1 JOIN (select 1,TRUE,1 union select 2, NULL,2 union select 3,FALSE,3) n2 on (1=1);
select n1.* = n2.* from (VALUES(1,TRUE,1) ,(2,TRUE,2) ,(1,TRUE,3)) n1 JOIN (select 1,TRUE,1 union select 2, NULL,2 union select 3,FALSE,3) n2 on (1=1);
select n1.* != n2.* from (VALUES(1,TRUE,1) ,(2,TRUE,2) ,(1,TRUE,3)) n1 JOIN (select 1,TRUE,1 union select 2, NULL,2 union select 3,FALSE,3) n2 on (1=1);

-- NULL column comparison
select n1.* < n2.* from (VALUES(NULL,NULL)) n1 JOIN (select 'test','test') n2 on (1=1);
select n1.* < n2.* from (VALUES('test','test')) n1 JOIN (select NULL,NULL) n2 on (1=1);
select n1.* < n2.* from (VALUES(NULL,NULL)) n1 JOIN (VALUES('test','test')) n2 on (1=1);
select n1.* < n2.* from (VALUES('test','test')) n1 JOIN (VALUES(NULL,NULL)) n2 on (1=1);

-- Composite NULL related queries #759
SELECT t1.id, t1.lastName, t2.* < t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL;
SELECT t1.id, t1.lastName FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL order by t2.* < t2.*;
SELECT t1.id, t1.lastName, t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL ORDER BY t2.* > t2.*;
SELECT t1.id, t1.lastName FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL ORDER BY t2.* < t2.*;
SELECT t1.id, MAX(t1.lastName) FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id ORDER BY t2.* > t2.*;
SELECT t1.id, MAX(t1.lastName) FROM names t1 LEFT JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id ORDER BY t2.* > t2.*;
SELECT t1.id, MAX(t1.lastName), t2.* > t2.* FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id;
SELECT t1.* > t2.* FROM names t1 LEFT JOIN (SELECT * FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*,t1.*;

-- The following query used to generate an incompatible operation error before !1163 because `AVG` type was being set to its parameter type which in this case is an INTEGER
select n1.* = n2.* from (select avg(id) from names where id = 3) n1, (select round(avg(id),1) from names where id = 3) n2;
