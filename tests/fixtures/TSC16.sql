#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSC16 : #282 : Test of ABS() function as well as lp_verify_structure() for LP_COLUMN_LIST

select abs(-2*id) from names;
select abs(-1.45);
select abs(1.45);
select abs(1.45-2.50);
select abs(1.23456789-9.87654321);
-- The below tests verify that lp_verify_structure() does handle various possible operand[0] types for LP_COLUMN_LIST
select abs(id+1) from names limit 1;            	-- LP_ADDITION
select abs(id-1) from names limit 1;                    -- LP_SUBTRACTION
select abs(id*1) from names limit 1;                    -- LP_MULTIPLICATION
select abs(id/1) from names limit 1;                    -- LP_DIVISION
select replace('abcd' || 'efgh', 'efgh', 'abcd');   -- LP_CONCAT
select replace('a' || 'b' || 'c', 'c', 'd');        -- LP_CONCAT whose first operand is LP_CONCAT : Test of #370
select abs(-id) from names limit 1;                     -- LP_NEGATIVE
select abs(id) from (select * from names) limit 1;      -- LP_DERIVED_COLUMN
select abs(id+1) from (select * from names) limit 1;    -- LP_ADDITION
select abs(id-1) from (select * from names) limit 1;    -- LP_SUBTRACTION
select abs(id*1) from (select * from names) limit 1;    -- LP_MULTIPLICATION
select abs(id/1) from (select * from names) limit 1;    -- LP_DIVISION
select abs(-id) from (select * from names) limit 1;     -- LP_NEGATIVE
