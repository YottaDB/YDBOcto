#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Test that DATE and TIME types work (TIME specified without any precision)
CREATE TABLE DATETIME (id INTEGER PRIMARY KEY, date1 DATE, time1 TIME);
SELECT date1 from DATETIME;
SELECT time1 from DATETIME;
SELECT id::DATE from DATETIME;
SELECT id::TIME from DATETIME;
SELECT date1::integer from DATETIME;
SELECT date1::numeric from DATETIME;
SELECT date1::varchar from DATETIME;
SELECT date1::text from DATETIME;
SELECT date1::date from DATETIME;
SELECT date1::time from DATETIME;


-- Test that TIME(precision) (TIME type specified with precision) also works
CREATE TABLE DATETIME (id INTEGER PRIMARY KEY, date1 DATE, time1 TIME(10));
SELECT time1 from DATETIME;
SELECT time1::integer from DATETIME;
SELECT time1::numeric from DATETIME;
SELECT time1::varchar from DATETIME;
SELECT time1::text from DATETIME;
SELECT time1::date from DATETIME;
SELECT time1::time from DATETIME;

