#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- -- Error
-- View definition with alias section
create view TCV003v1 (TCV003v1_id,NULL,TCV003v1_lastname) as select names.* from names;
create view TCV003v1 (TCV003v1_id,'',TCV003v1_lastname) as select names.* from names;
create view TCV003v1 (TCV003v1_id,'asdf',TCV003v1_lastname) as select names.* from names;
--create view TCV003v1 (TCV003v1_id,"asd",TCV003v1_lastname) as select names.* from names; -- double quotes
create view TCV003v1 (TCV003v1_id,TCV003v1_id,TCV003v1_lastname) as select names.* from names;
create view TCV003v1 (TCV003v1_id,TCV003v1_firstname,TCV003v1_id) as select id,firstname,lastname from names;
create view TCV003v1 (TCV003v1_id,TCV003v1_id) as select id from names; -- ERROR:  CREATE VIEW specifies more column names than columns
create view TCV003v1 (TCV003v1_col1) as select (values(lastname,firstname)) from names; -- ERROR:  subquery must return only one column
-- Static values at view definition with aliases
create view TCV003v1 (TCV003v1_id,TCV003v1_id,TCV003v1_lastname) as select 1,'Zero','Cool'; -- ERROR:  column "TCV003v1_id" specified more than once
create view TCV003v1 (TCV003v1_id,TCV003v1_id) as select 1; -- ERROR:  CREATE VIEW specifies more column names than columns
-- Values usage
create view TCV003v1 (TCV003v1_col1,TCV003v1_col2,TCV003v1_col3) as select (values(1,'Zero','Cool')) from names; -- ERROR:  subquery must return only one column
create view TCV003v1 (TCV003v1_col1,TCV003v1_col2,TCV003v1_col3) as select values(1,'Zero','Cool') from names; -- ERROR:  syntax error at or near "("
create view TCV003v1 as select id as n1_id,firstname,lastname as n1_id from names n1; -- column "n1_id" specified more than once --
-- Set operation
create view TCV003v1 (TCV003v1_firstname,TCV003v1_lastname,TCV003v1_id) as select firstname,lastname from names union select firstname,lastname from names; -- ERROR:  CREATE VIEW specifies more column names than columns

-- View definition without alias section
-- Values usage
create view TCV003v1 as select (values(lastname,firstname)) from names; -- values -- ERROR:  subquery must return only one column
-- Static values at view definition
create view TCV003v1 as select 1,'Zero','Cool'; -- column "?column?" specified more than once --
create view TCV003v1 as select 1 as n1_id,'Zero' as n1_firstname, 'Cool' as n1_firstname; -- ERROR:  column "n1_firstname" specified more than once
create view TCV003v1 as values((1,'Zero','Cool'),(1,'Z','C')); -- column "column1" has pseudo-type record (this one is more of values implementation error)
create view TCV003v1 as select(values(1,'Zero','Cool')) from names; -- ERROR:  subquery must return only one column
create view TCV003v1 as select values(1,'Zero','Cool') from names; -- ERROR:  syntax error at or near "("
-- Set operation
create view TCV003v1 as select firstname,firstname from names union select firstname,lastname from names; -- ERROR:  column "firstname" specified more than once
-- Static values with set opetion
create view TCV003v1 as select 'Zero','Zero1' union select 'Cool','Cool1'; -- ERROR:  column "?column?" specified more than once
create view TCV003v1 as select 'Zero','Zero1'union select 'Cool','Cool1' as col2; -- ERROR:  column "?column?" specified more than once
create view v3 as select * from names n1 right join names n2 on n1.firstname = n2.firstname; --  ERROR:  column "id" specified more than once


-- CREATE VIEW command specifies a list of column names but is only a partial list (i.e. the underlying SELECT query has a much
-- bigger select column list). And there is duplication of names in that CREATE VIEW column list. Should issue a
-- ERR_DUPLICATE_COLUMN error.
create table t1 (id integer);
create view TCV003v1(id1,id1) as select t1.id, t1.id, t1.id id2 from t1;

-- CREATE VIEW command specifies a list of column names that is a partial list with no duplication within the partial list but
-- there is a duplication between this and the remaining column names in the underlying SELECT query column list. Should issue
-- a ERR_DUPLICATE_COLUMN error.
create view TCV003v1(id1,id2) as select t1.id, t1.id, t1.id id2 from t1;

-- CREATE VIEW specifies a partial column list of names with no duplication. There is duplication of column names between 2
-- remaining columns in the underlying SELECT column list. Should issue a ERR_DUPLICATE_COLUMN error.
create view TCV003v1(id1,id2) as select t1.id, t1.id, t1.id id3, t1.id id3 from t1;

