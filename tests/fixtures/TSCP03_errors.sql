#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Test that boolean literals cannot be used in string operations i.e. below should error out
select true || false;

-- TODO: Below should error out (#559)
select id,(id = 2)::numeric from names;
select id,(id = 2)::date from names;
select id,(id = 2)::time from names;

-- Test binary operations (arithmetic, string concatenation) between boolean and non-boolean types ERRORs out
select id+(id=2) from names;
select id-(id=2) from names;
select id*(id=2) from names;
select id/(id=2) from names;
select id%(id=2) from names;

-- Test that boolean operations on non-boolean types issue errors
select id from names where (id OR id);
select id from names where (id AND id);
select id from names where (NOT id);
select firstname from names where (firstname OR firstname);
select firstname from names where (firstname AND firstname);
select firstname from names where (NOT firstname);

-- Boolean operations should issue error if one operand is not boolean type
select n2.id and EXISTS (select * from names n1) from names n2;
