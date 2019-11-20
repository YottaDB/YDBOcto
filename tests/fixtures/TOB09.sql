#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TOB08 : OCTO280 : ORDER BY column_number

-- Test with valid column numbers
select * from names order by 1 desc;
select * from names order by 2 desc;
select * from names order by 3;
select * from names order by 3,2,1 desc;
select * from names order by 3,2;
select * from names order by 2,1 desc;

-- Test that ORDER BY valid-column-numbers but inside expressions are treated as ORDER BY expression
select * from names order by 1+1 desc;
select * from names order by +2 desc;

-- Test with column number that is out of valid range
select * from names order by 4;
select * from names order by 0;
select * from names order by -1;

-- Test with fractional column numbers
select * from names order by 1.234;
select * from names order by -1.234;

-- Test with huge column numbers
select * from names order by 12345678901234567890;
select * from names order by -12345678901234567890;

-- Test with multiple column numbers with errors
select * from names order by 1.235,-12345678901234567890;
select * from names order by -1.235,12345678901234567890;
select * from names order by 1,2,-12345678901234567890;

