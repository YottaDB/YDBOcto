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

select n1.* = n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1.0) ,(2.0) ,(1.0)) n2 on (n1.column1=2);
select n1.* = n2.* from (select 'test' union select 'num' union select 'numtest') n1, (select 1 union select 2) n2;
select n1.* in (n2.*,n1.*) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1.0) ,(2.0) ,(1.0)) n2 on (n1.column1=2);
select n1.* in (select n2.*) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1.0) ,(2.0) ,(1.0)) n2 on (n1.column1=2);
select n2.* in (n2.* in (n2.*)) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(1.0) ,(2.0) ,(1.0)) n2 on (n1.column1=2);
select n1.* = n2.* from (VALUES(1,1) ,(2,2) ,(1,2)) n1 JOIN (select (1) union select (2) union select (1)) n2 on (n1.column1=2);
select n1.* = n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select 1,1 union select 2,2 union select 3,3) n2 on (n1.column1=2);
select n1.* in (n2.*,n1.*,n3.*) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(2) ,(3) ,(4)) n2 on (n1.column1=2) JOIN (VALUES(1,1), (2,2), (1,3)) n3 on (n1.column1=2); -- works in Postgres as n1.* matches with n1.* but since we don't do type validations at later stages of Octo we had to error this out at populate_data_type() itself.
select n1.* in (n2.*,n3.*,n1.*) from (VALUES(1) ,(2) ,(1)) n1 JOIN (VALUES(2) ,(3) ,(4)) n2 on (n1.column1=2) JOIN (VALUES(1,1), (2,2), (1,3)) n3 on (n1.column1=2); -- similar to the above query but this demonstrates that such queries error out in postgres as well.

SELECT t1.* in (t2.*)  FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL;
select n1.* in (n2.*) from (select 'test' union select 'num' union select 'numtest') n1, (select 1 union select 2) n2;
select n1.* in (n2.*) from (select 'test' union select 'num' union select 'numtest') n1, (select 1 union select 2) n2;
select n1.* in (n2.*) from (select 'test' union select NULL union select 'numtest') n1, (select 1 union select NULL) n2 group by n1.*,n2.* order by n1.*;

select n1.* = n2.* from (VALUES(1::VARCHAR) ,(2::VARCHAR) ,(1::VARCHAR)) n1 JOIN (VALUES(1) ,(2) ,(1)) n2 on (n1.column1='1');
select n1.* = n2.* from (VALUES(1::NUMERIC) ,(2::NUMERIC) ,(1::NUMERIC)) n1 JOIN (VALUES(1.0::INTEGER) ,(2.0::INTEGER) ,(1.0::INTEGER)) n2 on (n1.column1=2);

-- Following queries errors in Postgres but doesn't in Octo because of #848
select n1.* = n2.* from (VALUES(1::VARCHAR) ,(2::VARCHAR) ,(1::VARCHAR)) n1 JOIN (VALUES(NULL) ,(NULL) ,(NULL)) n2 on (n1.column1='1');
select n1.* = n2.* from (VALUES(1::VARCHAR) ,(2::VARCHAR) ,(1::VARCHAR)) n1 JOIN (VALUES('test') ,('test') ,('test')) n2 on (n1.column1='1');

select n1.* is not n1.* from names n1;
select n1.* is n1.* from names n1;
select NULL is n1.* from names n1;
select n1.* like 'test' from names n1;
select n1.* like NULL from names n1;
select 'test' like n1.* from names n1;
select n1.* similar to 'test' from names n1;
select n1.* similar to NULL from names n1;
select 'test' similar to n1.* from names n1;
select n1.* LIKE n1.* from names n1;
select NULL like n1.* from names n1;
select 'test' != n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select n2.* != 'test' from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select 'test' > n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select 'test' < n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select 'test' >= n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select 'test' <= n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');

-- NULL table column type should error
select n1.* = n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select (NULL) union select (NULL) union select (NULL)) n2 on (1=1);
select n1.* = n2.* from (VALUES(1.0) ,(2.0) ,(1.0)) n1 JOIN (select (NULL) union select (NULL) union select (NULL)) n2 on (1=1);
select n1.* = n2.* from (VALUES(true) ,(true) ,(true)) n1 JOIN (select (NULL) union select (NULL) union select (NULL)) n2 on (1=1);

-- Test IS TRUE/FALSE/UKNOWN
select n1.* is UNKNOWN from (VALUES(TRUE)) n1;
select n1.* is true from (VALUES(TRUE)) n1;
select n1.* is false from (VALUES(TRUE)) n1;
select n1.* is false from (VALUES(NULL)) n1;
select n1.* is not UNKNOWN from (VALUES(TRUE)) n1;
select n1.* is not true from (VALUES(TRUE)) n1;
select n1.* is not false from (VALUES(TRUE)) n1;
select n1.* is not false from (VALUES(NULL)) n1;

-- Mismatch among column type
select n1.* = n2.* from (VALUES(1,'test1',1.0) ,(2,'test2',2.0) ,(1,'test3',3.0)) n1 JOIN (select 1,'test1',1 union select 2, NULL,2 union select 3,'test3',3) n2 on (1=1);
select n1.* != n2.* from (VALUES(1,'test1',1) ,(2,'test2',2) ,(1,'test3',3)) n1 JOIN (select 1,'test1',1.0 union select 2, NULL,2.0 union select 3,'test3',3.0) n2 on (1=1);
select n1.* >= n2.* from (VALUES(1,'test1',1) ,(2,'test2',2) ,(1,'test3',3)) n1 JOIN (select 1,'test1',NULL union select 2, NULL,NULL union select 3,'test3',NULL) n2 on (1=1);
select n1.* NOT IN (n2.*) from (VALUES(1,'test1',1.0) ,(2,'test2',2.0) ,(1,'test3',3.0)) n1 JOIN (select 1,'test1',1 union select 2,NULL,2 union select 3,'test3',3) n2 on (1=1);

-- Mismatch in column count
select n1.* != n2.* from (VALUES(1,'test1') ,(2,'test2') ,(1,'test3')) n1 JOIN (select 1 union select 2 union select 3) n2 on (1=1);
select n1.* < n2.* from (VALUES(1) ,(2) ,(1)) n1 JOIN (select 1,'test1' union select 2,'test2' union select 3,'test3') n2 on (1=1);
select n1.* NOT IN (n2.*) from (VALUES(1,'test1',1.0) ,(2,'test2',2.0) ,(1,'test3',3.0)) n1 JOIN (select 1,'test1' union select 2,NULL union select 3,'test3') n2 on (1=1);

-- Subquery usages
-- All of the below queries are expected to error saying `ERR_TYPE_NOT_COMPATIBLE : Type TABLENAME.* not compatible for NULL type subquery comparison`
select n1.* NOT IN (select NULL union select NULL) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* NOT IN (VALUES(NULL), (NULL)) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* NOT IN (select NULL) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* NOT IN (select * from (select NULL)n1) from names n1;

select n1.* = (select NULL union select NULL) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* = (VALUES(NULL), (NULL)) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* = (select NULL) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* = (select * from (select NULL)n1) from names n1;

select n1.* < (select NULL union select NULL) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* > (VALUES(NULL), (NULL)) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* >= (select NULL) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* <= (select * from (select NULL)n1) from names n1;

-- All of the below queries are expected to error saying `ERR_TYPE_NOT_COMPATIBLE : Type TABLENAME.* not compatible for NULL type subquery comparison`
select (select NULL union select NULL) NOT IN (n1.*) from (VALUES('test1') ,('test2') ,('test3')) n1;
select (VALUES(NULL), (NULL)) NOT IN (n1.*) from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select NULL) NOT IN (n1.*) from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select * from (select NULL)n1) NOT IN (n1.*)from names n1;

select (select NULL union select NULL) = n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (VALUES(NULL), (NULL)) = n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select NULL) = n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select * from (select NULL)n2) = n1.* from names n1;

select (select NULL union select NULL) < n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (VALUES(NULL), (NULL)) > n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select NULL) >= n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select * from (select NULL)n1) <= n1.* from names n1;

-- All of the below queries are expected to error saying `ERR_TYPE_MISMATCH : Type mismatch: left TABLENAME.*, right (type of the subquery)`
select n1.* NOT IN (select 1 union select 2) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* NOT IN (VALUES(1.0), (2.0)) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* NOT IN (select 1) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* NOT IN (select * from (select 1)n1) from names n1;

select n1.* = (select 1 union select 2) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* = (VALUES(1.0), (2.0)) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* = (select 1) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* = (select * from (select 1)n1) from names n1;

select n1.* < (select 1 union select 2) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* > (VALUES(1.0), (2.0)) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* >= (select 1) from (VALUES('test1') ,('test2') ,('test3')) n1;
select n1.* <= (select * from (select 1)n1) from names n1;

-- `ERR_TYPE_MISMATCH : Type mismatch: left (type of the subquery), right TABLENAME.*`
select (select 1 union select 2) not in (n1.*) from (VALUES('test1') ,('test2') ,('test3')) n1;
select (VALUES(1.0), (2.0)) NOT IN (n1.*) from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select 1) IN (n1.*) from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select * from (select 1)n1) IN (n1.*) from names n1;

select (select 1 union select 2) = n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (VALUES(1.0), (2.0)) = n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select 1) != n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select * from (select 1)n2) != n1.* from names n1;

select (select 1 union select 2) < n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (VALUES(1.0), (2.0)) > n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select 1) >= n1.* from (VALUES('test1') ,('test2') ,('test3')) n1;
select (select * from (select 1)n2) <= n1.* from names n1;
