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

CREATE TABLE Persons (PersonID int primary key,LastName varchar(255) GLOBAL "^persons(keys(""personid""))" PIECE 1,FirstName varchar(255) GLOBAL "^persons(keys(""personid""))" PIECE 2,Address varchar(255) GLOBAL "^persons(keys(""personid""))" PIECE 3,City varchar(255) GLOBAL "^persons(keys(""personid""))" PIECE 4,ts TIMESTAMP WITH TIME ZONE GLOBAL "^persons(keys(""personid""))" PIECE 5,socnum INTEGER GLOBAL "^persons(keys(""personid""))" PIECE 6,foo BOOLEAN GLOBAL "^persons(keys(""personid""))" PIECE 7)GLOBAL "^persons(keys(""personid""))"DELIM "|";
select * from Persons;
create table tmp (id integer primary key, bln boolean) global "^a" readonly;
select * from tmp;
select true;
select false;
select true and true;
select * from Persons where foo = true;
select * from Persons where foo = false;
select * from Persons where foo AND true;
select * from tmp where bln > 'false';
select * from tmp where bln > 'true';
