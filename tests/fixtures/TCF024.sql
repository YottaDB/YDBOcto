#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF024 : OCTO562 : CREATE FUNCTION supports function overloading

CREATE FUNCTION OVERLOADEDFUNC (INTEGER, NUMERIC, VARCHAR) RETURNS VARCHAR AS $$^OVERLOADEDFUNC;
CREATE FUNCTION OVERLOADEDFUNC (NUMERIC, NUMERIC, VARCHAR) RETURNS VARCHAR AS $$^OVERLOADEDFUNC;
CREATE FUNCTION OVERLOADEDFUNC (VARCHAR, INTEGER) RETURNS VARCHAR AS $$^OVERLOADEDFUNC;
-- Since return types are not considered when distinguishing overloaded functions, only the most recently defined return type will be used
CREATE FUNCTION OVERLOADEDFUNC (BOOLEAN) RETURNS VARCHAR AS $$^OVERLOADEDFUNC;
DROP FUNCTION OVERLOADEDFUNC(BOOLEAN);
CREATE FUNCTION OVERLOADEDFUNC (BOOLEAN) RETURNS BOOLEAN AS $$^OVERLOADEDFUNC;

-- Parameter types: INTEGER, NUMERIC, VARCHAR
SELECT OVERLOADEDFUNC(1, 2.5, 'hello') FROM names;

-- Parameter types: NUMERIC, NUMERIC, VARCHAR
SELECT OVERLOADEDFUNC(1.2, 2.5, 'hello') FROM names;

-- Parameter types: VARCHAR, INTEGER
SELECT OVERLOADEDFUNC('hello', 2) FROM names;

-- Parameter type: BOOLEAN; return type: BOOLEAN
SELECT OVERLOADEDFUNC(true) FROM names;

-- Parameter type: BOOLEAN; return type: BOOLEAN
SELECT  * FROM names where firstname = OVERLOADEDFUNC(true);

