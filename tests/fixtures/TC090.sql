#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TC090 : OCTO1149 : keys()/values() are expanded after any M operator, not just "(" ", " and "_"

-- "age" places keys("age_source") in both the condition and the value position of each $SELECT arm.
-- "disp" places values("refrange") in the same two positions.
-- Before YDBOcto#1149 only a keys()/values() preceded by "(" or "," (or "_", for values()) was
-- expanded, so every reference that followed a ":" was emitted as is and the query failed with
-- %YDB-E-LVUNDEF, Undefined local variable: keys("age_source").
DROP TABLE IF EXISTS ranges;
CREATE TABLE ranges (
	id integer,
	age_source integer,
	age varchar(6) EXTRACT "$S(keys(""age_source"")<28:keys(""age_source"")_""D"",keys(""age_source"")<60:keys(""age_source"")\7_""W"",keys(""age_source"")<365:keys(""age_source"")\30_""M"",1:keys(""age_source"")\365_""Y"")",
	refrange varchar(13) PIECE 1,
	disp varchar(20) EXTRACT "$S(values(""refrange"")[""{"":$P(values(""refrange""),""{"",1)_"" - ""_$P(values(""refrange""),""{"",2),1:values(""refrange""))",
	-- The below must NOT be expanded: "mykeys(" is the tail of a longer M name, "^keys(" is a
	-- global variable name and "keys(x)" here is the content of an M string literal.
	neg1 varchar(20) EXTRACT "$$mykeys^TC090(keys(""id""))",
	neg2 varchar(20) EXTRACT "$G(^keys(""x""))",
	neg3 varchar(20) EXTRACT "$S(1:""keys(x)"")",
	PRIMARY KEY (id, age_source)
) GLOBAL "^ref(keys(""id""),keys(""age_source""))" DELIM "`" READONLY;

select * from ranges;
select id, age from ranges where age = '3M';
