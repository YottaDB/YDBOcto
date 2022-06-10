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

select n1.* = n2.* from names n1 JOIN names n2 on (n1.id=2);
select n1.* in (n1.*,n2.*) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1=2);
select n1.* = n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1=2);
select n1.* || NULL from (VALUES(1) ,(2) ,(1)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (n1.column1=2);
select n1.* in (n2.*) from (select 'test' union select NULL union select 'numtest') n1, (select 'nottest' union select NULL) n2 group by n1.*,n2.* order by n1.*; --sort-needed-check
select n1.* in (n2.*) from (select NULL union select NULL union select NULL) n1, (select 'nottest' union select NULL) n2 group by n1.*,n2.* order by n1.*; --sort-needed-check
select n1.* = NULL from (VALUES(1) ,(2) ,(1)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (n1.column1=2);
select NULL = n1.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (n1.column1=2);
select n1.* = n2.* from (VALUES(1::VARCHAR) ,(2::VARCHAR) ,(1::VARCHAR)) n1 JOIN (VALUES(1::VARCHAR) ,(2::VARCHAR) ,(1::VARCHAR)) n2 on (n1.column1='1');
select n1.* = n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1.0::INTEGER) ,(2.0::INTEGER) ,(1.0::INTEGER)) n2 on (n1.column1=2);
select n1.* = n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (VALUES('test') ,('test') ,('test')) n2 on (n1.column1='1');

-- The following query should not error as both the tables should have the same type. #849 tracks this issue.
-- select n1.* in (n2.*) from (select 1 union select NULL union select NULL) n1, (select NULL union select 1) n2 group by n1.*,n2.* order by n1.*;

-- The following query is a counter example which is supposed to error out for comparison being done between INTEGER type (n1.*) and STRING_TYPE (n2.*) but
--  with #833 implementation the we see an error but the data types mentioned for n2.* is NULL which is incorrect. #849 tracks this issue.
-- select n1.* in (n2.*) from (select 1 union select NULL union select NULL) n1, (select NULL union select 'test') n2 group by n1.*,n2.* order by n1.*;

select n1.* = n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1=2);
select n1.* != n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1=2);
select n1.* != n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1=2);
select n1.* IS NULL from (VALUES(1) ,(2) ,(1)) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1=2);
select n1.* IS NOT NULL from (VALUES(1) ,(2) ,(1)) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1=2);
select n1.* IS NOT NULL from (VALUES('test') ,('test') ,('test')) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1='test');
select n1.* IS NULL from (VALUES('test') ,('test') ,('test')) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1='test');
select n1.* is not null from names n1;
select n1.* is null from names n1;
select NULL != n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select n2.* != NULL from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select NULL > n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select NULL < n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select NULL >= n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select NULL <= n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select n2.* > NULL from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select n2.* < NULL from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select n2.* >= NULL from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select n2.* <= NULL from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select n1.* in (NULL,n2.*) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1=2);
select n1.* in (n1.*,NULL) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1=2);
select n1.* in (NULL) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1=2);
select NULL in (n1.*,NULL) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1=2);


select ALL n1.* != n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select ALL n1.* <= n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select ALL n1.* >= n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select ALL n1.* > n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select ALL n1.* < n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');

select n1.* > n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1=2);
select n1.* < n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1=2);

-- NULL table column type comparison
select n1.* = n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select (NULL) union select (NULL) union select (NULL)) n2 on (1=1);


-- Multiple column tables
select n1.* = n2.* from (VALUES(1,1) ,(2,2) ,(1,2)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (1=1);
select n1.* = n2.* from (VALUES(1,1,1) ,(2,2,2) ,(1,2,3)) n1 JOIN (select 1,1,1 union select 2,2,2 union select 3,3,3) n2 on (1=1);
select n1.* != n2.* from (VALUES(1,1) ,(2,2) ,(1,2)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (1=1);
select n1.* != n2.* from (VALUES(1,1,1) ,(2,2,2) ,(1,2,3)) n1 JOIN (select 1,1,1 union select 2,2,2 union select 3,3,3) n2 on (1=1);

select n1.* < n2.* from (VALUES(1,1) ,(2,2) ,(1,2)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (1=1);
select n1.* > n2.* from (VALUES(1,1,1) ,(2,2,2) ,(1,2,3)) n1 JOIN (select 1,1,1 union select 2,2,2 union select 3,3,3) n2 on (1=1);

select n1.* <= n2.* from (VALUES(1,1) ,(2,2) ,(1,2)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (1=1);
select n1.* >= n2.* from (VALUES(1,1,1) ,(2,2,2) ,(1,2,3)) n1 JOIN (select 1,1,1 union select 2,2,2 union select 3,3,3) n2 on (1=1);


select n1.* IN (n2.*) from (VALUES(1,1) ,(2,2) ,(1,2)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (1=1);
select n1.* NOT IN (n2.*) from (VALUES(1,1,1) ,(2,2,2) ,(1,2,3)) n1 JOIN (select 1,1,1 union select 2,2,2 union select 3,3,3) n2 on (1=1);

select n1.* IN (n2.*,n1.*) from (VALUES(1,1) ,(2,2) ,(1,2)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (1=1);
select n1.* NOT IN (n2.*,n1.*) from (VALUES(1,1,1) ,(2,2,2) ,(1,2,3)) n1 JOIN (select 1,1,1 union select 2,2,2 union select 3,3,3) n2 on (1=1);


select n1.* = n2.* from (VALUES(1,'test1') ,(2,'test2') ,(1,'test3')) n1 JOIN (select 1,'test1' union select 2,'test2' union select 3,'test3') n2 on (1=1);
select n1.* = n2.* from (VALUES(1,'test1',1) ,(2,'test2',2) ,(1,'test3',3)) n1 JOIN (select 1,'test1',1 union select 2, NULL,2 union select 3,'test3',3) n2 on (1=1);
select n1.* != n2.* from (VALUES(1,'test1') ,(2,'test2') ,(1,'test3')) n1 JOIN (select 1,'test1' union select 2,'test2' union select 3,'test3') n2 on (1=1);
select n1.* != n2.* from (VALUES(1,'test1',1) ,(2,'test2',2) ,(1,'test3',3)) n1 JOIN (select 1,'test1',1 union select 2, NULL,2 union select 3,'test3',3) n2 on (1=1);

select n1.* < n2.* from (VALUES(1,'test1') ,(2,'test2') ,(1,'test3')) n1 JOIN (select 1,'test1' union select 2,'test2' union select 3,'test3') n2 on (1=1);
select n1.* <= n2.* from (VALUES(1,'test1') ,(2,'test2') ,(1,'test3')) n1 JOIN (select 1,'test1' union select 2,'test2' union select 3,'test3') n2 on (1=1);

select n1.* IN (n2.*) from (VALUES(1,'test1') ,(2,'test2') ,(1,'test3')) n1 JOIN (select 1,'test1' union select 2,'test2' union select 3,'test3') n2 on (1=1);
select n1.* NOT IN (n2.*) from (VALUES(1,'test1',1) ,(2,'test2',2) ,(1,'test3',3)) n1 JOIN (select 1,'test1',1 union select 2,NULL,2 union select 3,'test3',3) n2 on (1=1);

select n1.* IN (n2.*,n1.*) from (VALUES(1,'test1') ,(2,'test2') ,(1,'test3')) n1 JOIN (select 1,'test1' union select 2,'test2' union select 3,'test3') n2 on (1=1);
select n1.* NOT IN (n2.*,n1.*) from (VALUES(1,'test1',1) ,(2,'test2',2) ,(1,'test3',3)) n1 JOIN (select 1,'test1',1 union select 2,NULL,2 union select 3,'test3',3) n2 on (1=1);


