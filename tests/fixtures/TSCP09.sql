#################################################################
#								#
# Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSCP19 : OCTO370/OCTO623 : Test of string concatenation in select column list

SELECT 'a_' || firstname || '_' || lastname || '_b' FROM names;

-- TSCP19 : OCTO623 : The below tests verify that lp_verify_structure() handles various operand[1] types for LP_CONCAT
--   All the possible types are enumerated in "lp_verify_value()".
--   The only exception is LP_WHERE which is not considered possible as an operand[1] for LP_CONCAT
--   and hence is not tested below.

SELECT firstname || (1 + 0) FROM names;						-- LP_ADDITION
SELECT firstname || (3 - 1) FROM names;						-- LP_SUBTRACTION
SELECT firstname || (2 * 4) FROM names;						-- LP_MULTIPLICATION
SELECT firstname || (9 / 3) FROM names;						-- LP_DIVISION
SELECT firstname || (9 % 5) FROM names;						-- LP_MODULO
SELECT firstname || -5 FROM names;						-- LP_NEGATIVE
SELECT firstname || +6 FROM names;						-- LP_FORCE_NUM
SELECT firstname || 'xyz' FROM names;						-- LP_CONCAT
SELECT firstname || lastname FROM names;					-- LP_COLUMN_ALIAS
SELECT firstname || case when true then 7 else 0 end FROM names;		-- LP_CASE
SELECT firstname || ABS(-1) FROM names;						-- LP_FUNCTION_CALL
SELECT firstname || COALESCE(1,0) FROM names;					-- LP_COALESCE_CALL
SELECT firstname || GREATEST(1,0) FROM names;					-- LP_GREATEST
SELECT firstname || LEAST(1,0) FROM names;					-- LP_LEAST
SELECT firstname || NULLIF(1,0) FROM names;					-- LP_NULL_IF
SELECT firstname || COUNT(lastname) FROM names GROUP BY firstname;		-- LP_AGGREGATE_FUNCTION_COUNT
SELECT firstname || COUNT(*) FROM names GROUP BY firstname;			-- LP_AGGREGATE_FUNCTION_COUNT_ASTERISK
SELECT firstname || COUNT(names.*) FROM names GROUP BY firstname;		-- LP_AGGREGATE_FUNCTION_COUNT_TABLE_ASTERISK
-- The below query also tests [OCTO629 : Incorrect results when string concatenation operator || is used with aggregate functions]
SELECT firstname || AVG(id) FROM names GROUP BY firstname;			-- LP_AGGREGATE_FUNCTION_AVG
SELECT firstname || MIN(id) FROM names GROUP BY firstname;			-- LP_AGGREGATE_FUNCTION_MIN
SELECT firstname || MAX(id) FROM names GROUP BY firstname;			-- LP_AGGREGATE_FUNCTION_MAX
SELECT firstname || SUM(id) FROM names GROUP BY firstname;			-- LP_AGGREGATE_FUNCTION_SUM
SELECT firstname || COUNT(DISTINCT lastname) FROM names GROUP BY firstname;	-- LP_AGGREGATE_FUNCTION_COUNT_DISTINCT
SELECT firstname || COUNT(DISTINCT names.*) FROM names GROUP BY firstname;	-- LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK
SELECT firstname || AVG(DISTINCT id) FROM names GROUP BY firstname;		-- LP_AGGREGATE_FUNCTION_AVG_DISTINCT
SELECT firstname || SUM(DISTINCT id) FROM names GROUP BY firstname;		-- LP_AGGREGATE_FUNCTION_SUM_DISTINCT
SELECT firstname || id FROM (SELECT * FROM names) n1;				-- LP_DERIVED_COLUMN
SELECT 'abcd' || 'efgh';							-- LP_VALUE
SELECT 'abcd' || '00'::integer;							-- LP_COERCE_TYPE
-- Note: No need to test LP_BOOLEAN_* types in lp_verify_value() because any BOOLEAN operand would be typecast
-- to a string (i.e. LP_COERCE_TYPE plan would be ahead of it as the operand of LP_CONCAT).
SELECT 'abcd' || (SELECT 1);							-- LP_SELECT_QUERY
SELECT 'abcd' || (SELECT 1 EXCEPT SELECT 2);					-- LP_SET_OPERATION

-- Test of YDBOcto#1038
select ('1' || 'Acid') > 'Burn';
select (1 || 'Acid') > 'Burn';

