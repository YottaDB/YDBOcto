#################################################################
#								#
# Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	#
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
SELECT day(NULL);
SELECT dayofmonth(NULL);
-- Date type usage
--  Dates >= 10
SELECT day(date'2017-06-15');
SELECT dayofmonth(date'2017-06-15');
--  Dates < 10
SELECT day(date'2021-12-01');
SELECT dayofmonth(date'2021-12-01');
SELECT day(cast(now() as date));
SELECT dayofmonth(cast(now() as date));
