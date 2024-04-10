#################################################################
#								#
# Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF034 : OCTO884 : Octo supports SQL keywords and parenless function names in CREATE/DROP FUNCTION statements

-- Can create/drop functions with names of SQL keywords whose signature doesn't match octo-seed function
CREATE FUNCTION IF NOT EXISTS TRUNCATE(INTEGER, INTEGER, INTEGER) RETURNS NUMERIC AS $$TRUNCATE^TCF034;
SELECT TRUNCATE(1, 1, 1);

DROP FUNCTION IF EXISTS TRUNCATE(INTEGER, INTEGER, INTEGER);
CREATE FUNCTION TRUNCATE(INTEGER, INTEGER, INTEGER) RETURNS NUMERIC AS $$TRUNCATE^TCF034;
SELECT TRUNCATE(1, 1, 1);

-- Can create/drop parenless functions
CREATE FUNCTION IF NOT EXISTS current_userf() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
select current_userf();

DROP FUNCTION IF EXISTS current_userf;
CREATE FUNCTION current_userf() RETURNS VARCHAR AS $$pgUser^%ydboctopgfunctions;
select current_userf();
