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
-- TC091 : OCTO1112 : keys() in an EXTRACT is expanded when it follows the "_" concatenation operator

-- "globalname" concatenates a string literal with keys("file_identifier"). Before YDBOcto#1149 a
-- keys() was expanded only when it was preceded by "(" or "," ("_" was accepted for values() alone),
-- so this reference was emitted as is and the query failed with
-- %YDB-E-LVUNDEF, Undefined local variable: keys("file_identifier").
DROP TABLE IF EXISTS orders_file_specimens;
CREATE TABLE orders_file_specimens (
	order_number varchar(20),
	file_identifier varchar(1),
	internal_specimen_number varchar(20),
	globalname varchar EXTRACT """^GRES""_keys(""file_identifier"")",
	specimen_type varchar(10) PIECE 1,
	PRIMARY KEY (order_number, file_identifier, internal_specimen_number)
) GLOBAL "^GREQ(keys(""order_number""),""R"",keys(""file_identifier""),keys(""internal_specimen_number""))" DELIM "`" READONLY;

select * from orders_file_specimens;
select globalname from orders_file_specimens where file_identifier = '2';
