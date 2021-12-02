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

-- TDTF07 : OCTO288 : NULL returned for invalid values in each date/time field

-- Invalid year
SELECT DATE_FORMAT('10000-00-00 12:30:30', '%Y-%m-%d %H:%i:%s');
-- Invalid month
SELECT DATE_FORMAT('1900-25-00 12:30:30', '%Y-%m-%d %H:%i:%s');
-- Invalid day
SELECT DATE_FORMAT('1900-00-32 12:30:30', '%Y-%m-%d %H:%i:%s');
-- Invalid hour
SELECT DATE_FORMAT('1900-00-00 25:30:30', '%Y-%m-%d %H:%i:%s');
-- Invalid minute
SELECT DATE_FORMAT('1900-00-00 12:80:30', '%Y-%m-%d %H:%i:%s');
-- Invalid second
SELECT DATE_FORMAT('1900-00-00 12:30:80', '%Y-%m-%d %H:%i:%s');
