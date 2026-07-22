#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TDTT113: OCTO1095 : Verify floor division correctness for timestamp/time arithmetic around epoch
-- Exact repro from the issue
SELECT timestamp'1970-01-01T00:00:00.000000' - time'00:00:00.000001';
-- 0s boundary (epoch), +/-1 microsecond
SELECT timestamp'1970-01-01 00:00:00' - time'00:00:00.000001';
SELECT timestamp'1969-12-31 23:59:59.999999' + time'00:00:00.000001';
SELECT timestamp'1970-01-01 00:00:00.000001' - time'00:00:00.000001';
-- -1s boundary, +/-1 microsecond
SELECT timestamp'1969-12-31 23:59:59' - time'00:00:00.000001';
SELECT timestamp'1969-12-31 23:59:58.999999' + time'00:00:00.000001';
SELECT timestamp'1969-12-31 23:59:59' + time'00:00:00.000001';
-- -2s boundary, +/-1 microsecond
SELECT timestamp'1969-12-31 23:59:58' - time'00:00:00.000001';
SELECT timestamp'1969-12-31 23:59:57.999999' + time'00:00:00.000001';
-- +1s boundary, +/-1 microsecond
SELECT timestamp'1970-01-01 00:00:01' - time'00:00:00.000001';
SELECT timestamp'1970-01-01 00:00:00.999999' + time'00:00:00.000001';
-- Comparisons/ordering across the epoch boundary (regression from #382/aa163c57 -- must not regress)
SELECT timestamp'1969-12-31 23:59:59.999998' < timestamp'1969-12-31 23:59:59.999999';
SELECT timestamp'1969-12-31 23:59:59.999999' < timestamp'1970-01-01 00:00:00.000000';
-- Direct literal round-trip at the boundary (parse + print path, not just arithmetic path)
SELECT timestamp'1969-12-31 23:59:59.999999';
SELECT timestamp'1969-12-31 23:59:59.999998';
