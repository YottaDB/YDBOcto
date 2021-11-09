#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TNO01 : OCTO603 : names schema : Assert failure when NOT is used on functions like LEAST, GREATEST, NULLIF etc.

select id from names where not least(true,false);	-- LP_LEAST
select id from names where not greatest(true,false);	-- LP_GREATEST
select id from names where not nullif(false,true);	-- LP_NULL_IF
select id from names where not coalesce(true,false);	-- LP_COALESCE

select id from names where not boolfunc(id);		-- LP_FUNCTION_CALL

select id from names where not case when id < 3 then true else false end;	-- LP_CASE

-- Note: Aggregate functions cannot have boolean operands so no need to test the following LP_* types in lp_action_type.hd
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_COUNT)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_COUNT_ASTERISK)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_COUNT_TABLE_ASTERISK)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_SUM)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_AVG)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_MIN)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_MAX)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_COUNT_DISTINCT)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_COUNT_DISTINCT_TABLE_ASTERISK)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_SUM_DISTINCT)
-- LP_ACTION_TYPE(LP_AGGREGATE_FUNCTION_AVG_DISTINCT)


