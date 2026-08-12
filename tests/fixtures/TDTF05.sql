#################################################################
#								#
# Copyright (c) 2021-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDTF05 : OCTO288 : Test day(), dayofmonth()
--  Dates >= 10
SELECT day('2017-06-15');
SELECT dayofmonth('2017-06-15');
--  Dates < 10
SELECT day('2021-12-01');
SELECT dayofmonth('2021-12-01');
-- Invalid dates
SELECT day('2021-02-29');
SELECT dayofmonth('2021-02-29');
SELECT day('2020-02-30');
SELECT dayofmonth('2021-02-30');
SELECT day('2021-12-32');
SELECT dayofmonth('2021-12-32');
SELECT day('2021-11-31');
SELECT dayofmonth('2021-11-31');
-- A day field of 0 returns 0, matching MySQL's DAYOFMONTH(), except when the whole date is zero
SELECT day('1999-06-00');
SELECT dayofmonth('1999-06-00');
SELECT day('0000-06-00');
SELECT dayofmonth('0000-06-00');
SELECT day('1999-00-00');
SELECT dayofmonth('1999-00-00');
SELECT day('0000-00-00');
SELECT dayofmonth('0000-00-00');
SELECT day(NULL);
SELECT dayofmonth(NULL);
-- Date type usage
--  Dates >= 10
SELECT day(date'2017-06-15');
SELECT dayofmonth(date'2017-06-15');
--  Dates < 10
SELECT day(date'2021-12-01');
SELECT dayofmonth(date'2021-12-01');
-- Regression test: the prior C implementation returned a string whose declared length was always the
-- format string's maximum possible width rather than the number of characters actually written, so
-- comparisons/concatenation against its own output silently failed even though it displayed correctly.
SELECT day('2017-06-15') = '15';
SELECT day('2021-12-01') = '1';
SELECT day('2017-06-15') || 'X';
SELECT day('2021-12-01') || 'X';
SELECT day(date'2017-06-15') = '15';
SELECT day(date'2021-12-01') = '1';
SELECT day(date'2017-06-15') || 'X';
SELECT day(date'2021-12-01') || 'X';
-- Malformed/adversarial input coverage: analogous to the now-removed TDTF06/TDTF08/TDTF09/TDTF11, which
-- exercised these cases against the removed DATE_FORMAT() function's C-based parser. All of these must
-- resolve safely (no crash/hang) to either NULL or the correct day, since day()'s M-based parser has no
-- fixed-size buffers to overflow.
-- Overlong numeric fields (was TDTF06: no stack smashing)
SELECT day('123456789123456789-00-00 -25:30:30');
SELECT day('1900-123456789123456789-00 -25:30:30');
-- Extra hyphens (was TDTF08)
SELECT day('----1900-00-00 12:30:30');
SELECT day('1900-00-----1 12:30:30');
-- Non-numeric separators (was TDTF09)
SELECT day('1900-s00-00 12:30:40');
SELECT day('1900-00-00 12:30:[:30');
-- 0 month / 0 day (was TDTF11)
SELECT day('2016-01-00');
SELECT day('3099-00-28');
