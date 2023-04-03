#################################################################
#                                                               #
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.  #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

-- create view
create view v1 as select * from names;
create view v2 as select * from names;

-- display relation \d
\d;

-- display view relation \dv
\dv;

-- display view relation with view name \d view_name
\d v1;
\d v2;

-- Verify that after a drop view the dropped view isn't seen with the display relation command
drop view v1;
drop view v2;
\d;
\dv;
\d v1;
\d v2;

-- More tests on \d view_name to check definition output
create view v1 as select 1;
create view v2 as select firstname, lastname from names;
create view v3 (id1,firstname1,lastname1,id2,firstname2,lastname2) as select * from names,names n2;
create view v4 as (VALUES (1,2,3),(1,2,3));
create view v5 as (VALUES (1,'test','test2'));
create view v6 as (VALUES(1,'test','test2'),(2,'test3','test4'));
create view v7 as select * from (VALUES(1,'test','test2'),(2,'test3','test4')) as val7;
create view v8 (v1_firstname) as select firstname from names union select lastname from names;
create view v9 as select * from (select firstname, lastname from names)n1;
create view v10 as select (select 1) as sub1 from names;
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
create view v33 as select * from (select * from names) order by lastname;
create view v34 as select * from (select lastname from names union select firstname from names) order by lastname;
create view v35 as select * from (values(1)) order by 1;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379082949
create view v36 as select 1 from (select 1 union select 1);
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379091825
create view v37 as select CASE WHEN NULL THEN 1.1 WHEN 1.0+3.3=4.3 THEN 1.2 ELSE 9.9 end;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379101951
create view v38 as select 1 IN (2,3);
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379107070
create view v39 as select sum(mybool::integer) from (select id=2 as mybool from names) n1;
create view v40 as select count(*) from (select id=2 from names) n1;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380391980
create view v41 as select id from names n1 where (select n1.id from names n2 where n2.id > 5) is not null;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380390157
create view v42 as SELECT NULL FROM names GROUP BY firstname HAVING NULL!='hello' ORDER BY 1;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380388151
create view v43 as SELECT NULL FROM names GROUP BY 1 HAVING NULL!='hello';
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380380124
create view v44 as SELECT 1.2 FROM names GROUP BY 1 HAVING 1.2=1.2 ORDER BY 1;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379097653
create view v45 as SELECT * FROM names a WHERE a.firstName != (SELECT NULL from names limit 2);
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379108907
create view v46 as SELECT 10 + (select NULL)::INTEGER FROM names;
create view v47 as SELECT ABS((select id from names where firstname = 'noname'));
create view v48 as select 'test' NOT IN ((select n1.firstname),NULL) from names n1;
create view v49 as select (case when id > (select 1) then (select 1) else (select 0) end)-1 as idbool from names;
create view v50 as select (case (select 1) when (select 1) then 'true' end)as sub, TRUE;
create view v51 as select * from (select ((values(lastname))) = 'Burn' from names) as n1;
create view v52 as select * from (select ((select lastname)) = 'Burn' from names) as n1;
create view v53 as select * from (select ((select 1)) = 1 from names) as n1;
create view v54 as select * from names as n1 where (select n2.id % 2 != 1 from names n2 where n1.id = n2.id);
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380382091
create view v55 as select all alias1.firstname from names as names  natural join names as alias1 where (names.id is null) order by alias1.firstname;
create view v56 as SELECT 1 FROM names x HAVING EXISTS(SELECT t1.id, MAX(t1.lastName) FROM names t1 LEFT JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE t1.id <= 1 and t2.lastName IS NULL GROUP BY t2.*, t1.id having 0 IN (SELECT t1.id FROM names y ORDER BY t2.*,t1.id limit 1));
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379108907
create view v57 as SELECT * FROM (SELECT id as A FROM names) n1 NATURAL JOIN (SELECT id as B FROM names) n2;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1384324106
create view v58 as select count(DISTINCT n1.*) from (select id,firstname from names) n1;
create view v59 as SELECT firstname || COUNT(DISTINCT names.*) FROM names GROUP BY firstname; -- LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1384324108
create view v60 as select col1 from (select id as col1 from names) n2;
create view v61 as select firstname from (select lastname as ln1,firstname from names)n1 order by exists (select firstname ||'test' from (select firstname as ln1 from names) n2 group by ln1);
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1384324110
create view v62 as SELECT * FROM (SELECT firstname,COUNT(id) FROM names n1 GROUP BY firstname) n2;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380436701
create view v63 as SELECT * from names where 'Baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' != 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380375577
create view v64 as SELECT * FROM (SELECT * FROM names) AS abcd(a2,b2,c2) LIMIT 1;
create view v65 as SELECT * FROM names WHERE (id = (SELECT x FROM (VALUES(id)) AS tbl(x)));
create view v66 as SELECT ALL alias1.id FROM (select * from (values (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', NULL), (5, 'Zero', 'Cool')) as names(id,firstname,lastname)) AS names  NATURAL JOIN names AS alias1 WHERE (((4) + -(-1)) IS NOT NULL) ORDER BY alias1.id;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380541896
create view v67 as select `alias(names)`.* from names as `alias(names)`;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1382354577
create view v68 as select ALL n1.* != n2.* from (VALUES('test') ,('test') ,('test')) n1 JOIN (select ('test') union select ('test') union select ('test')) n2 on (n1.column1='test');
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1382352931
CREATE VIEW v69 as SELECT CASE WHEN id IS NULL THEN 'is null' WHEN id=0 THEN 'is zero' WHEN id=1 THEN 'is one' end FROM ((SELECT 0 AS id) UNION (SELECT 1 AS id) UNION (SELECT NULL AS id)) n1 order by id;
CREATE VIEW v70 as SELECT id IS NULL FROM ((SELECT 0 AS id) UNION (SELECT 1 AS id)) n1;
create view v84 as select distinct lastname from names;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379103370
create view v85 as select exists (select * from names);
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380399080
create view v86 as SELECT * FROM (SELECT * FROM (SELECT id, id%2 AS newid FROM names) n1) n2 ORDER BY newid;
create view v87 as SELECT * FROM (SELECT * FROM (SELECT id, id%2 FROM names) n1) n2;


\d v1;
\d v2;
\d v3;
\d v4;
\d v5;
\d v6;
\d v7;
\d v8;
\d v9;
\d v10;
\d v11;
\d v12;
\d v13;
\d v14;
\d v15;
\d v16;
\d v17;
\d v18;
\d v19;
\d v20;
\d v21;
\d v22;
\d v23;
\d v24;
\d v25;
\d v26;
\d v27;
\d v28;
\d v29;
\d v30;
\d v31;
\d v32;
\d v33;
\d v34;
\d v35;
\d v36;
\d v37;
\d v38;
\d v39;
\d v40;
\d v41;
\d v42;
\d v43;
\d v44;
\d v45;
\d v46;
\d v47;
\d v48;
\d v49;
\d v50;
\d v51;
\d v52;
\d v53;
\d v54;
\d v55;
\d v56;
\d v57;
\d v58;
\d v59;
\d v60;
\d v61;
\d v62;
\d v63;
\d v64;
\d v65;
\d v66;
\d v67;
\d v68;
\d v69;
\d v70;
\d v84;
\d v85;
\d v86;
\d v87;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375473876
create view a as select 1;
create view b as select * from a;
\d a;
\d b;

-- Parmless function
CREATE FUNCTION PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;
CREATE FUNCTION THREEPARMFUNC(VARCHAR,INTEGER,INTEGER) RETURNS VARCHAR AS $$threeparmfunc^functions;
create view c AS select * FROM names WHERE THREEPARMFUNC(PARMLESSFUNC(),2,3) = 'SUCCESS';
\d c;


