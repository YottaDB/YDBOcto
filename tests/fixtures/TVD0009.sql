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
# YDBOctoVistA#28 Externally Formatted Dates
-- Query with times
select INSTALL_START_TIME, INSTALL_START_TIME_E, INSTALL_COMPLETE_TIME, INSTALL_COMPLETE_TIME_E from INSTALL LIMIT 10;

-- Query with time at .24 (midnight)
select STARTTIME, STARTTIME_E, ENDTIME, ENDTIME_E from SDEC_APPOINTMENT where ENDTIME::varchar LIKE '%.24';

-- Query with Inexact dates
select distinct DATE_OF_ONSET,DATE_OF_ONSET_E from PROBLEM1 where DATE_OF_ONSET is not NULL;
