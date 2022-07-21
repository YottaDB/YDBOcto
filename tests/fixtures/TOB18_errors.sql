#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Following query validates ORDER BY ambiguity resolution logic
select 'Zero' != 'Zero' as firstname,firstname from names order by firstname;

-- Following queries are from #868 issue comments and description. They demonstrated the difference which lead to single error per query change.
select id,firstname from names group by 1 order by firstname;
select id,firstname as firstname from names group by 1 order by firstname;

-- Following query ensures that 1+id is not replacing 1+id. We know that the replacement hasn't happened as the query issues `ERR_ORDER_BY_SELECT_DISTINCT` saying ORDER BY `id` must appear in select list
SELECT DISTINCT 2+id,lastname FROM names ORDER BY id;

-- Test alias name ambiguity in FROM/JOIN column list
select 1, 2 from names n1, names n2 order by firstname;

-- Test column name ambiguity in SELECT column list
select 1 as ambiguous, 2 as ambiguous from names order by ambiguous;

-- Misc
SELECT * FROM names GROUP BY 1 ORDER BY firstname;
