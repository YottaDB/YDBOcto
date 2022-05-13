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
CREATE FUNCTION ZEXT(VARCHAR,INTEGER,INTEGER) RETURNS VARCHAR AS $$ZEXT^Q6F;

-- A shortened table to show the usecase
CREATE TABLE QAUDITCEVENT (
ID VARCHAR PRIMARY KEY,
DATELABEL VARCHAR GLOBAL "^qAuditC(keys(""ID""),""EventDescr"",""DateLabel"")" PIECE 1,
EVENTNAME VARCHAR GLOBAL "^qAuditC(keys(""ID""),""EventDescr"",""EventName"")" PIECE 1
)
 GLOBAL "^qAuditC(keys(""ID""),""EventDescr"")";

-- making an sql expression with grouping by year:
select ZEXT(DATELABEL, 1, 4), count (*) from QAUDITCEVENT group by ZEXT(DATELABEL, 1,4);

