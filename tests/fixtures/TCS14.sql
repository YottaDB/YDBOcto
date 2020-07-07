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

-- TCS14 : OCTO544 : Assertion failure and Errors when IN is used in SELECT column list

-- Below queries used to generate a RPARENMISSING error
SELECT CASE WHEN 1 IN (1) THEN 2 END;
SELECT CASE WHEN 1 IN (2) THEN 2 END;
SELECT id IN (2) FROM names;
SELECT id IN (2,5,8) FROM names;
SELECT id NOT IN (0,3,7,0) FROM names;

-- Below queries worked fine even before the OCTO544 code fixes but are included in case these are not already tested
SELECT id != 5 FROM names;
SELECT id = 5 FROM names;

