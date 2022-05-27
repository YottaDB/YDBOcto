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

-- creating a function for cutting "year" from datelabel
DROP FUNCTION IF EXISTS ZEXT(VARCHAR,INTEGER,INTEGER);
CREATE FUNCTION ZEXT(VARCHAR,INTEGER,INTEGER) RETURNS VARCHAR AS $$ZEXT^Q6F;
--this is a shortened table to show the error
DROP TABLE IF EXISTS QAUDITCEVENT;
CREATE TABLE QAUDITCEVENT
(
ID VARCHAR PRIMARY KEY,
DATELABEL VARCHAR,
EVENTNAME VARCHAR
) GLOBAL "^qAuditC";

SELECT XECUTE_M_CODE("kill ^qAuditC");
SELECT XECUTE_M_CODE("set ^qAuditC(1)=""20200101|New Year"",^qAuditC(2)=""20200229|Leap Day""");
SELECT XECUTE_M_CODE("set ^qAuditC(3)=""20210101|New Year""");

-- making an sql expression with grouping by year:
SELECT ZEXT(DATELABEL, 1, 4), count (*) from QAUDITCEVENT group by ZEXT(DATELABEL, 1,4);
SELECT ZEXT(DATELABEL, 1, 4), count (*) from QAUDITCEVENT group by 1;
