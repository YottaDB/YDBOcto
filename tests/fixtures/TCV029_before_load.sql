#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

create view v1_1 as select * from names;
select * from v1_1;
create view v2_1 ("id 1","firstname 1","lastname 1",id2,firstname2,lastname2) as select * from names,names n2;
select * from v2_1;
create view v3_1 (v1_firstname) as select firstname as "v1 firstname" from names union select lastname as "v1 lastname" from names;
select * from v3_1;
create view v4_1 as select firstname as "v1 firstname" from names union select lastname as "v1 lastname" from names;
select * from v4_1;
create view v5_1 as select * from composite;
select * from v5_1;

-- Views on relations in octo-seed.sql
create view v6_1 as select 1;
select * from v6_1;

create view v7_1 as select * from pg_type;
select * from v7_1;

create view v8_1 as select abs(1) from names limit 1;
select * from v8_1;

-- Views which depend on other Views
create view b1 as select * from names;
select * from b1;

create view a1 as select * from b1;
select * from a1;

-- Following usage failed when multiple functions of the same name but different parameter types were created
CREATE FUNCTION SAMEVALUE(INTEGER) RETURNS INTEGER AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(NUMERIC) RETURNS NUMERIC AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(VARCHAR) RETURNS VARCHAR AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(BOOLEAN) RETURNS BOOLEAN AS $$samevalue^functions;
create view f1 as select samevalue(1);
select * from f1;
select samevalue(1);

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375758802
create view a as select 1;
create view b as select * from a;
select * from b;
select * from a;

-- Parmless function
CREATE FUNCTION PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;
CREATE FUNCTION THREEPARMFUNC(VARCHAR,INTEGER,INTEGER) RETURNS VARCHAR AS $$threeparmfunc^functions;
create view c AS select * FROM names WHERE THREEPARMFUNC(PARMLESSFUNC(),2,3) = 'SUCCESS';
select * from c;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375502307
create view v9_1 as select * from octoonerowtable;
create view v10_1 as select * from v9_1;
create view v11_1 as select * from v9_1;
select * from v9_1;
select * from v10_1;
select * from v11_1;



create view v1 as select 1;
create view v2 as select firstname, lastname from names;
create view v3 (id1,firstname1,lastname1,id2,firstname2,lastname2) as select * from names,names n2;
create view v4 as (VALUES (1,2,3),(1,2,3));
create view v5 as (VALUES (1,'test','test2'));
create view v6 as (VALUES(1,'test','test2'),(2,'test3','test4'));
create view v7 as select * from (VALUES(1,'test','test2'),(2,'test3','test4')) as val7;
create view v8 (v1_firstname) as select firstname from names union select lastname from names;
create view v9 as select * from (select firstname, lastname from names)n1;
-- create view v10 as select (select 1) as sub1 from names;
create view v11 as select n1.* from names n1;
create view v12 as select firstname from names group by firstname having firstname != 'Cool';
create view v13 as select firstname from names group by 1 having firstname != 'Cool';
create view v14 as select firstname from names group by 1 having firstname != 'Cool' order by 1;
create view v15 as select firstname from names group by 1 having firstname != 'Cool' order by firstname;
create view v16 as select count(firstname) from names;
create view v17 as select * from names limit 1;
create view v18 as select distinct * from names;
create view v19 as select firstname from names order by firstname desc, lastname asc;
create view v20 as select firstname from names order by exists(select id from names order by id desc);
create view v21 as values(1,2,3);
create view v22 as select id from (select id from names)n1;
create view v23(a1,a2,a3,a4,a5,a6) as select * from (VALUES (1,2,3)) n1, (VALUES (3,4,5)) n2;
create view v24 as select n1.* from (select id from names)n1 inner join (select id from names)n2 on n1.id>0;
create view v25 ("id 1","firstname 1","lastname 1",id2,firstname2,lastname2) as select * from names,names n2;
create view v26 (v1_firstname) as select firstname as "v1 firstname" from names union select lastname as "v1 lastname" from names;
create view v27 as select firstname as "v1 firstname" from names union select lastname as "v1 lastname" from names;
create view v28 as select * from v27;
create view v29 as select * from v28;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375820974
create view v30 as SELECT * FROM ((SELECT * FROM names) UNION (SELECT NULL AS id, NULL AS firstname, NULL AS lastname)) n1 WHERE (id = 0) OR NOT (id = 0) OR (id IS NULL) ORDER BY n1.id;
create view v31 as SELECT * FROM ((SELECT * FROM names) UNION (SELECT NULL AS id, NULL AS firstname, NULL AS lastname)union (select * from names)) n1 WHERE (id = 0) OR NOT (id = 0) OR (id IS NULL) ORDER BY n1.id;
create view v32 as (select * from names UNION select * from names) UNION ALL (select * from names UNION select * from names);

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375783812
create view v33 as select * from (select * from names) order by lastname;
create view v34 as select * from (select lastname from names union select firstname from names) order by lastname;
create view v35 as select * from (values(1)) order by 1;
select * from v33;
select * from v34;
select * from v35;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379082949
create view v36 as select 1 from (select 1 union select 1);
select * from v36;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379091825
create view v37 as select CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
select * from v37;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379101951
create view v38 as select 1 IN (2,3);
select * from v38;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379107070
create view v39 as select sum(mybool::integer) from (select id=2 as mybool from names) n1;
select * from v39;
create view v40 as select count(*) from (select id=2 from names) n1;
select * from v40;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380391980
create view v41 as select id from names n1 where (select n1.id from names n2 where n2.id > 5) is not null;
select * from v41;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380390157
create view v42 as SELECT NULL FROM names GROUP BY firstname HAVING NULL!='hello' ORDER BY 1;
select * from v42;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380388151
create view v43 as SELECT NULL FROM names GROUP BY 1 HAVING NULL!='hello';
select * from v43;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380380124
create view v44 as SELECT 1.2 FROM names GROUP BY 1 HAVING 1.2=1.2 ORDER BY 1;
select * from v44;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379097653
create view v45 as SELECT * FROM names a WHERE a.firstName != (SELECT NULL from names limit 2);
select * from v45;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379108907
create view v46 as SELECT 10 + (select NULL)::INTEGER FROM names;
select * from v46;
create view v47 as SELECT ABS((select id from names where firstname = 'noname'));
select * from v47;
create view v48 as select 'test' NOT IN ((select n1.firstname),NULL) from names n1;
select * from v48;
create view v49 as select (case when id > (select 1) then (select 1) else (select 0) end)-1 as idbool from names;
select * from v49;
create view v50 as select (case (select 1) when (select 1) then 'true' end)as sub, TRUE;
select * from v50;
create view v51 as select * from (select ((values(lastname))) = 'Burn' from names) as n1;
select * from v51;
create view v52 as select * from (select ((select lastname)) = 'Burn' from names) as n1;
select * from v52;
create view v53 as select * from (select ((select 1)) = 1 from names) as n1;
select * from v53;
create view v54 as select * from names as n1 where (select n2.id % 2 != 1 from names n2 where n1.id = n2.id);
select * from v54;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380382091
create view v55 as select all alias1.firstname from names as names  natural join names as alias1 where (names.id is null) order by alias1.firstname;
select * from v55;
create view v56 as SELECT 1 FROM names x HAVING EXISTS(SELECT t1.id, MAX(t1.lastName) FROM names t1 LEFT JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id having 0 IN (SELECT t1.id FROM names y ORDER BY t2.*,t1.id limit 1));
select * from v56;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379108907
create view v57 as SELECT * FROM (SELECT id as A FROM names) n1 NATURAL JOIN (SELECT id as B FROM names) n2;
select * from v57;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1384324106
create view v58 as select count(DISTINCT n1.*) from (select id,firstname from names) n1;
select * from v58;
create view v59 as SELECT firstname || COUNT(DISTINCT names.*) FROM names GROUP BY firstname; -- LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK
select * from v59;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1384324108
create view v60 as select col1 from (select id as col1 from names) n2;
select * from v60;
create view v61 as select firstname from (select lastname as ln1,firstname from names)n1 order by exists (select firstname ||'test' from (select firstname as ln1 from names) n2 group by ln1);
select * from v61;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1384324110
create view v62 as SELECT * FROM (SELECT firstname,COUNT(id) FROM names n1 GROUP BY firstname) n2;
select * from v62;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380436701
create view v63 as SELECT * from names where 'Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' != 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
select * from v63;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380375577
create view v64 as SELECT * FROM (SELECT * FROM names) AS abcd(a2,b2,c2) LIMIT 1;
select * from v64;
create view v65 as SELECT * FROM names WHERE (id = (SELECT x FROM (VALUES(id)) AS tbl(x)));
select * from v65;
create view v66 as SELECT ALL alias1.id FROM (select * from (values (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', NULL), (5, 'Zero', 'Cool')) as names(id,firstname,lastname)) AS names  NATURAL JOIN names AS alias1 WHERE (((4) + -(-1)) IS NOT NULL) ORDER BY alias1.id;
select * from v66;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380541896
create view v67 as select `alias(names)`.* from names as `alias(names)`;
select * from v67;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1382354577
create view v68 as select ALL n1.* != n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
select * from v68;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1382352931
CREATE VIEW v69 as SELECT CASE WHEN id IS NULL THEN 'is null' WHEN id=0 THEN 'is zero' WHEN id=1 THEN 'is one' end FROM ((SELECT 0 AS id) UNION (SELECT 1 AS id) UNION (SELECT NULL AS id)) n1 order by id;
select * from v69;
CREATE VIEW v70 as SELECT id IS NULL FROM ((SELECT 0 AS id) UNION (SELECT 1 AS id)) n1;
select * from v70;

-- Column level constraints
create table tmp80 (id integer unique, name text, value numeric check (value > 0));
insert into tmp80 values(1,'first',1.2);
insert into tmp80 values(2,'second',2.2);
insert into tmp80 values(3,'third',0); -- Fails due to value being less than 0
insert into tmp80 values(2,'third',3.2); -- Fails due to id being not unique
create view v80 as select * from tmp80;
select * from v80;

-- test column level check constraint with a name is accepted
create table tmp81 (id integer, name text, value numeric constraint name1 check (value > 0));
insert into tmp81 values(1,'first',1.2);
insert into tmp81 values(2,'second',2.2);
insert into tmp81 values(3,'third',0); -- Fails due to value being less than 0
create view v81 as select * from tmp81;
select * from v81;

-- table level constraints
-- test table level check constraint without a name is accepted
create table tmp82 (id integer, check (id > 0),unique(id));
insert into tmp82 values(1);
insert into tmp82 values(2);
insert into tmp82 values(0); -- Fails due to id being less than 0
insert into tmp82 values(1); -- Fails due to id being not unique
create view v82 as select * from tmp82;
select * from v82;

-- test table level check constraint with a name is accepted
create table tmp83 (id integer, constraint name1 check (id > 0));
insert into tmp83 values(1);
insert into tmp83 values(2);
insert into tmp83 values(0); -- Fails due to id being less than 0
create view v83 as select * from tmp83;
select * from v83;

create view v84 as select distinct lastname from names;
select * from v84;

-- PIECE, START and ENDPOINT usage
create table tmp85 (id integer primary key start 0 endpoint 3, firstname varchar(30), lastname varchar(30)) global "^names";
create view v85 as select * from tmp85;
-- -- both the following queries should have same output
select * from v85;
select * from tmp85;

create table tmp86 (id integer primary key start 0 endpoint 3, firstname varchar(30) piece 2, lastname varchar(30) piece 1) global "^names";
create view v86 as select * from tmp86;
-- -- both the following queries should have same output
select * from v86;
select * from tmp86;

-- Auto incrementing column or IDENTITY column (GENERATED ALWAYS AS IDENTITY)
create table tmp87 (id int generated always as identity, str text);
INSERT INTO tmp87(str) VALUES('first'); -- Auto increment value assigned for ID
INSERT INTO tmp87(str) VALUES('second'); -- Auto increment value assigned for ID
create view v87 as select * from tmp87;
-- -- both the following queries should have same output
select * from v87;
select * from tmp87;

-- Auto incrementing column or IDENTITY column (GENERATED BY DEFAULT AS IDENTITY)
create table tmp88 (id int generated by default as identity, str text);
INSERT INTO tmp88(str) VALUES('first'); -- Auto increment value assigned for ID
INSERT INTO tmp88(id,str) VALUES(3,'second'); -- 3 assigned for ID
INSERT INTO tmp88(str) VALUES('third'); -- Auto increment value assigned for ID and its value should be 2
create view v88 as select * from tmp88;
-- -- both the following queries should have same output
select * from v88;
select * from tmp88;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379103370
create view v89 as select exists (select * from names);
select * from v89;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380399080
create view v90 as SELECT * FROM (SELECT * FROM (SELECT id, id%2 AS newid FROM names) n1) n2 ORDER BY newid;
create view v91 as SELECT * FROM (SELECT * FROM (SELECT id, id%2 FROM names) n1) n2;
select * from v90;
select * from v91;


-- Skipping the following keywords for now as the above list of cases seem good enough for validating keyword usage
-- OPTIONAL_END,
-- OPTIONAL_DELIM,
-- OPTIONAL_EXTRACT,
-- OPTIONAL_CASCADE,
-- OPTIONAL_RESTRICT,
-- OPTIONAL_KEY_NUM,
-- OPTIONAL_ADVANCE, /* Corresponds to nixed ADVANCE keyword. Not deleted for backward compatibility just in case. */
-- OPTIONAL_XREF_INDEX,        // not sure if this should be here; gets populated through LP
-- OPTIONAL_BOOLEAN_EXPANSION, // indicates that this statement is part of an OR boolean expression expansion to BNF form
-- OPTIONAL_STARTINCLUDE,
-- OPTIONAL_READONLY,
-- OPTIONAL_READWRITE,
-- OPTIONAL_KEEPDATA,
-- OPTIONAL_AIM_TYPE,

