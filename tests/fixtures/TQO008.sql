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

-- TQO008 : #1141 : Only comparison operators are accepted inside OPERATOR(); anything else is a syntax error
-- Octo supports only the OPERATOR() form of the comparison operators. Arithmetic, string concatenation and the
-- pattern matching operators are deliberately not supported in this form, so each query below is expected to
-- fail with a syntax error that points at the offending operator.

select id from names where id operator(pg_catalog.+) 1 = 4;
select id from names where id operator(pg_catalog.-) 1 = 2;
select id from names where id operator(pg_catalog.*) 1 = 3;
select id from names where id operator(pg_catalog./) 1 = 3;
select id from names where id operator(pg_catalog.%) 2 = 1;
select firstname from names where firstname operator(pg_catalog.||) 'x' = 'Zerox';
select firstname from names where firstname operator(pg_catalog.~~) 'Zero';
select firstname from names where firstname operator(pg_catalog.!~~) 'Zero';
select firstname from names where firstname operator(pg_catalog.~) 'Zero';
select firstname from names where firstname operator(pg_catalog.!~) 'Zero';
select id from names where id operator() 3;
select id from names where id operator(pg_catalog.) 3;
select id from names where id operator(pg_catalog.pg_class.=) 3;
