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

drop table names;
select * from v1_1;
select * from v2_1;
select * from v3_1;
select * from v4_1;
drop table composite;
select * from v5_1;

-- Views on relations in octo-seed.sql
drop table octoonerowtable;
select * from v6_1;

drop table pg_type;
select * from v7_1;

drop function abs(integer);
select * from v8_1;

-- Views which depend on other Views
drop view b1;
select * from a1;
select * from b1;

-- Following usage failed when multiple functions of the same name but different parameter types were created
drop function samevalue(integer);
select * from f1;
select samevalue(1) from names;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375758802
drop view a;
select * from b;
select * from a;

-- Parmless function
select * from c;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375502307
drop view v9_1;
select * from v10_1;
drop view v10_1;
drop view v9_1;
select * from v11_1;
drop view v11_1;
select * from v9_1;
drop view v9_1;

select * from v1;
select * from v2;
select * from v3;
select * from v4;
select * from v5;
select * from v6;
select * from v7;
select * from v8;
select * from v9;
-- select * from v10;
select * from v11;
select * from v12;
select * from v13;
select * from v14;
select * from v15;
select * from v16;
select * from v17;
select * from v18;
select * from v19;
select * from v20;
select * from v21;
select * from v22;
select * from v23;
select * from v24;
select * from v25;
select * from v26;
select * from v27;
select * from v28;
select * from v29;
select * from v30;
select * from v31;
select * from v32;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1375783812
drop table names;
select * from v35;
select * from v34;
select * from v33;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379082949
select * from v36;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379091825
select * from v37;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379101951
select * from v38;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379107070
select * from v39;
select * from v40;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380391980
select * from v41;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380390157
select * from v42;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380388151
select * from v43;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380380124
select * from v44;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379097653
select * from v45;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379108907
select * from v46;
select * from v47;
select * from v48;
select * from v49;
select * from v50;
select * from v51;
select * from v52;
select * from v53;
select * from v54;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380382091
select * from v55;
select * from v56;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379108907
select * from v57;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1384324106
select * from v58;
select * from v59;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1384324108
select * from v60;
select * from v61;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1384324110
select * from v62;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380436701
select * from v63;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380375577
select * from v64;
select * from v65;
select * from v66;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380541896
select * from v67;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1382354577
select * from v68;
-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1382352931
select * from v69;
select * from v70;

-- Column level constraints
drop table tmp80; -- Ensure view dependency is intact
select * from v80;
-- Ensure the original table constraints are not affected
insert into tmp80 values(3,'third',0); -- Fails due to value being less than 0
insert into tmp80 values(2,'third',3.2); -- Fails due to id being not unique
-- test column level check constraint with a name is accepted
drop table tmp81; -- Ensure view dependency is intact
select * from v81;
insert into tmp81 values(3,'third',0); -- Fails due to value being less than 0
-- table level constraints
-- test table level check constraint without a name is accepted
drop table tmp82; -- Ensure view dependency is intact
select * from v82;
insert into tmp82 values(0); -- Fails due to id being less than 0
insert into tmp82 values(1); -- Fails due to id being not unique
-- test table level check constraint with a name is accepted
drop table tmp83; -- Ensure view dependency is intact
select * from v83;
insert into tmp83 values(0); -- Fails due to id being less than 0

select * from v84;

-- PIECE, START and ENDPOINT usage
drop table tmp86;
drop table tmp85;
select * from v85;
select * from v86;
drop view v85;
drop view v86;
drop table tmp85;
drop table tmp86;
-- Check names is not affected by above queries as that is the base table for tmp86 and tmp85
select * from names;

-- Auto incrementing column or IDENTITY column (GENERATED ALWAYS AS IDENTITY)
-- -- both the following queries should have same output
-- -- and the max value for the column `id` seen by the two queries should be `2`
select * from v87;
select * from tmp87;
INSERT INTO tmp87(str) VALUES('second'); -- Auto increment value assigned for ID
-- -- both the following queries should have same output
-- -- and the new row added should have `id` column value as 3
select * from v87;
select * from tmp87;

-- Auto incrementing column or IDENTITY column (GENERATED BY DEFAULT AS IDENTITY)
-- -- both the following queries should have same output
-- -- and the value of `id` column here should be 1,3,2
select * from v88;
select * from tmp88;
INSERT INTO tmp88(id,str) VALUES(6,'fourth'); -- 6 assigned for ID
INSERT INTO tmp88(str) VALUES('fifth'); -- Auto increment value assigned for ID and the value should be 3
-- -- both the following queries should have same output
-- -- and the output rows should have 1,3,2,6,3 as values for `id` column
select * from v88;
select * from tmp88;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1379103370
select * from v89;

-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1380399080
select * from v90;
select * from v91;
