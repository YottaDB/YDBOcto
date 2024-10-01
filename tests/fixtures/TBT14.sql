#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select 1=1;
select id=id from names;
select true;
select false;

drop table if exists film;
CREATE TABLE film (PersonID int,LastName varchar(255),FirstName varchar(255),Address varchar(255),City varchar(255),foo BOOLEAN);

INSERT INTO film VALUES (1,'SurnameA','NameA','Samutprakarn','Thailand',true);
INSERT INTO film VALUES (2,'SurnameB','NameB','Samutprakarn','Thailand',false);
INSERT INTO film VALUES (3,'SurnameC','NameC','Samutprakarn','Thailand',true);
INSERT INTO film VALUES (4,'SurnameD','NameD','Samutprakarn','Thailand',false);
INSERT INTO film VALUES (5,'SurnameE','NameE','Samutprakarn','Thailand',true);
INSERT INTO film VALUES (6,'SurnameF','NameF','Samutprakarn','Thailand',true);
INSERT INTO film VALUES (7,'SurnameG','NameG','Samutprakarn','Thailand',false);
INSERT INTO film VALUES (8,'SurnameH','NameH','Samutprakarn','Thailand',true);

select * from film;

select * from film where foo=true;
select * from film where foo=false;
