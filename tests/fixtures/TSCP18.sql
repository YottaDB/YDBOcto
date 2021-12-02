#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSCP18 : OCTO288 : Support functions now(), lpad(), localtimestamp(), localtime(), current_timestamp(), current_time()
-- Note that some of those functions have already been implemented previously and are tested elsewhere.
SELECT lpad('a', 5);
SELECT lpad('aaa', 2);

-- The following queries are disabled due to timing discrepancies that may occur during
-- the crosscheck process, i.e. due to queries being called successively in Octo and Postgres,
-- the result of the underlying $$^%ydboctofcurrenttimestamp call may differ by 1 second
-- between the two cases.
-- SELECT cast(localtimestamp as varchar(20));
-- SELECT current_timestamp::varchar(9);
-- SELECT current_time::varchar(9);
-- SELECT cast(now() as varchar(20)); -- truncates to nearest second since Octo and Psql may be run a few milliseconds apart
-- SELECT localtime::varchar(9);
