#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSC19 : OCTO370/OCTO623 : Test of string concatenation in select column list

SELECT 'a_' || firstname || '_' || lastname || '_b' FROM names LIMIT 1;

-- TSC19 : OCTO623 : The below tests verify that lp_verify_structure() handles various operand[1] types for LP_CONCAT
--   All the possible types are enumerated in "lp_verify_value()".
--   The only exception is LP_WHERE which is not considered possible as an operand[1] for LP_CONCAT
--   and hence is not tested below.

SELECT firstname || (1 + 0) FROM names LIMIT 1;						-- LP_ADDITION
SELECT firstname || (3 - 1) FROM names LIMIT 1;						-- LP_SUBTRACTION
SELECT firstname || (2 * 4) FROM names LIMIT 1;						-- LP_MULTIPLICATION
SELECT firstname || (9 / 3) FROM names LIMIT 1;						-- LP_DIVISION
SELECT firstname || (9 % 5) FROM names LIMIT 1;						-- LP_MODULO
SELECT firstname || -5 FROM names LIMIT 1;						-- LP_NEGATIVE
SELECT firstname || +6 FROM names LIMIT 1;						-- LP_FORCE_NUM
SELECT firstname || 'xyz' FROM names LIMIT 1;						-- LP_CONCAT
SELECT firstname || lastname FROM names LIMIT 1;					-- LP_COLUMN_ALIAS
SELECT firstname || case when true then 7 else 0 end FROM names LIMIT 1;		-- LP_CASE
SELECT firstname || ABS(-1) FROM names LIMIT 1;						-- LP_FUNCTION_CALL
SELECT firstname || COALESCE(1,0) FROM names LIMIT 1;					-- LP_COALESCE_CALL
SELECT firstname || GREATEST(1,0) FROM names LIMIT 1;					-- LP_GREATEST
SELECT firstname || LEAST(1,0) FROM names LIMIT 1;					-- LP_LEAST
SELECT firstname || NULLIF(1,0) FROM names LIMIT 1;					-- LP_NULL_IF
SELECT firstname || COUNT(*) FROM names GROUP BY firstname LIMIT 1;			-- LP_AGGREGATE_FUNCTION_COUNT_ASTERISK
SELECT firstname || COUNT(lastname) FROM names GROUP BY firstname LIMIT 1;		-- LP_AGGREGATE_FUNCTION_COUNT
SELECT firstname || AVG(id) FROM names GROUP BY firstname LIMIT 1;			-- LP_AGGREGATE_FUNCTION_AVG
SELECT firstname || MIN(id) FROM names GROUP BY firstname LIMIT 1;			-- LP_AGGREGATE_FUNCTION_MIN
SELECT firstname || MAX(id) FROM names GROUP BY firstname LIMIT 1;			-- LP_AGGREGATE_FUNCTION_MAX
SELECT firstname || SUM(id) FROM names GROUP BY firstname LIMIT 1;			-- LP_AGGREGATE_FUNCTION_SUM
SELECT firstname || COUNT(DISTINCT lastname) FROM names GROUP BY firstname LIMIT 1;	-- LP_AGGREGATE_FUNCTION_COUNT_DISTINCT
SELECT firstname || AVG(DISTINCT id) FROM names GROUP BY firstname LIMIT 1;		-- LP_AGGREGATE_FUNCTION_AVG_DISTINCT
SELECT firstname || SUM(DISTINCT id) FROM names GROUP BY firstname LIMIT 1;		-- LP_AGGREGATE_FUNCTION_SUM_DISTINCT
SELECT firstname || id FROM (SELECT * FROM names) LIMIT 1;				-- LP_DERIVED_COLUMN
SELECT 'abcd' || 'efgh';								-- LP_VALUE
SELECT 'abcd' || '00'::integer;								-- LP_COERCE_TYPE
-- Note: No need to test LP_BOOLEAN_* types in lp_verify_value() because any BOOLEAN operand would be typecast
-- to a string (i.e. LP_COERCE_TYPE plan would be ahead of it as the operand of LP_CONCAT).
SELECT 'abcd' || (SELECT 1);								-- LP_SELECT_QUERY
SELECT 'abcd' || (SELECT 1 EXCEPT SELECT 2);						-- LP_SET_OPERATION

