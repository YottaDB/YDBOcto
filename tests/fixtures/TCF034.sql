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

-- TCF034 : OCTO884 : Octo supports SQL keywords and parenless function names in CREATE/DROP FUNCTION statements

-- Can create/drop functions with names of SQL keywords
DROP FUNCTION TRUNCATE(INTEGER, INTEGER);
CREATE FUNCTION IF NOT EXISTS TRUNCATE(INTEGER, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
SELECT TRUNCATE(1, 1);

DROP FUNCTION IF EXISTS TRUNCATE(INTEGER, INTEGER);
CREATE FUNCTION TRUNCATE(INTEGER, INTEGER) RETURNS NUMERIC AS $$^%ydboctofTRUNCATE;
SELECT TRUNCATE(1, 1);

-- Can create/drop parenless functions
DROP FUNCTION current_user;
CREATE FUNCTION IF NOT EXISTS current_user() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
select current_user;

DROP FUNCTION IF EXISTS current_user;
CREATE FUNCTION current_user() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
select current_user;
