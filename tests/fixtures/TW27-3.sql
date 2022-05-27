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
-- Test various ENDPOINTs with AIM: Test empty string
DROP TABLE IF EXISTS `ORDER_STATUS`;
CREATE TABLE `ORDER_STATUS` (
 `ORDER_STATUS_ID` INTEGER PRIMARY KEY START 0 ENDPOINT '""',
 `NAME` CHARACTER(20) NOT NULL GLOBAL "^ORD(100.01,keys(""ORDER_STATUS_ID""),0)" PIECE 1,
 `SHORT_NAME` CHARACTER(4) GLOBAL "^ORD(100.01,keys(""ORDER_STATUS_ID""),0)" PIECE 2,
 `ABBREVIATION` CHARACTER(3) GLOBAL "^ORD(100.01,keys(""ORDER_STATUS_ID""),.1)" EXTRACT "$E($G(^ORD(100.01,keys(""ORDER_STATUS_ID""),.1)),1,245)",
 `MASTER_ENTRY_FOR_VUID` CHARACTER(3) GLOBAL "^ORD(100.01,keys(""ORDER_STATUS_ID""),""VUID"")" PIECE 2,
 `VUID` CHARACTER(20) GLOBAL "^ORD(100.01,keys(""ORDER_STATUS_ID""),""VUID"")" PIECE 1
)
GLOBAL "^ORD(100.01,keys(""ORDER_STATUS_ID""))"
DELIM "^";
SELECT * FROM `ORDER_STATUS` WHERE SHORT_NAME = 'flag';
