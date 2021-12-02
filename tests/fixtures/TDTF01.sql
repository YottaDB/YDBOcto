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

-- TDTF01 : OCTO288 : Support functions now(), lpad(), date_add(), coalesce(), date_format(), isnull(), current_user(), truncate(), day()
-- Note that some of those functions have already been implemented previously and are tested elsewhere.
SELECT lpad('a', 5, 'b');
SELECT lpad('abb', 10, 'c');
SELECT lpad('ab', 1, 'd');
SELECT lpad('a', 5, 'bcd');
SELECT lpad('abb', 10, 'cdef');
SELECT lpad('ab', 1, 'defgh');
-- TODO: NULL cases are disabled until issue #816 is resolved
-- SELECT lpad(NULL, 1, 'defgh');
-- SELECT lpad('ab', NULL, 'defgh');
-- SELECT lpad('ab', 1, NULL);
SELECT truncate(1.525, 0); -- same as one-argument version
SELECT truncate(1.525, 1);
SELECT truncate(1, 0); -- should work for integers as well as floats
SELECT truncate(1, 1);
SELECT truncate(1.575, 2); -- should truncate down, not round
SELECT truncate(-1.525, 2); -- should truncate up, not round
SELECT truncate(-1.575, 2); -- should truncate up, not round
SELECT truncate(155, -1);
SELECT truncate(155, -2);
SELECT truncate(155, -3);
SELECT truncate(155, -43);
SELECT truncate(155, 44);
 -- Truncate the following to the nearest 10 seconds since Octo and Psql may be run a few milliseconds apart,
 -- leading to failures if one is run in second x and then other in second x+1.
SELECT concat('TOEPOCH', current_time);
SELECT concat('TOEPOCH', localtime);
SELECT concat('TOEPOCH', now());
SELECT concat('TOEPOCH', current_timestamp);
SELECT concat('TOEPOCH', localtimestamp);
