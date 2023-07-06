#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC072 : OCTO519 : Test x.y column name syntax
CREATE TABLE `X.Y` (`ID` INTEGER CONSTRAINT X.Y_PKEY PRIMARY KEY) GLOBAL "^xy(keys(""id""))" DELIM "|" READWRITE;
SELECT * FROM x.y;
\d x.y;

DROP TABLE IF EXISTS X.Y;
CREATE TABLE X.Y (id INTEGER CONSTRAINT XY_PKEY PRIMARY KEY, firstName VARCHAR(30), lastName VARCHAR(30)) GLOBAL "^names";
SELECT * FROM x.y;
\d x.y;

INSERT INTO x.y VALUES (12, 'First12', 'Last12'), (13, 'First13', 'Last13');
SELECT * FROM x.y;
SELECT * FROM x.y x1, x.y x2 where X1.id = 2;
DELETE FROM x.y where lastname = 'Cool';
UPDATE x.y SET id = 7, firstname = 'Sunny' WHERE lastname = 'Cool';
TRUNCATE x.y;

select x.y.z from octoonerowtable as n1(x.y.z); -- Error: x.y.z format not accepted for aliases
select x.y from octoonerowtable as n1(x.y); -- Error: x.y format not accepted for aliases
select x from octoonerowtable as n1(x);

create table a.b.c (id integer);
\d a.b.c;

truncate x.y.z; -- Error: x.y.z format not accepted for TRUNCATE
create table x (id integer constraint x.y.z primary key);  -- Error: x.y.z format not accepted for CONSTRAINTs
create table x (id integer constraint x.y primary key);  -- Error: x.y format not accepted for CONSTRAINTs

insert into tmp (tmp.id) values (1); -- Error: x.y format not accepted for aliases
update tmp set tmp.id = 1; -- Error: x.y format not accepted in UPDATE columns
