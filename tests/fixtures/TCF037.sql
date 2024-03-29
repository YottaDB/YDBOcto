#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF037 : OCTO816 and OCTO1010 : Test NULL/INTEGER literals in functions with large number of parameters

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/816#note_1514490913
-- Test that below query does not take forever to run
CREATE FUNCTION MAXARGS(INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER) RETURNS NUMERIC AS $$^MAXARGS;
SELECT MAXARGS(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, id) FROM names;

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/816#note_1515900594
-- Test that below query does not take forever to run
CREATE FUNCTION MAXARGS(NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC) RETURNS NUMERIC AS $$^MAXARGS;
SELECT MAXARGS(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, id) FROM names;

-- Test ERR_UNKNOWN_FUNCTION error in function with lots of parameters for query in https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/816#note_1514490913
SELECT MAXARGS(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, firstname) FROM names;

-- Test ERR_UNKNOWN_FUNCTION error in function with lots of parameters for query in https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/816#note_1515900594
SELECT MAXARGS(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, firstname) FROM names;

-- Test ERR_FUNCTION_NOT_UNIQUE error in function with lots of parameters for query in https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/816#note_1514490913
CREATE FUNCTION MAXARGS(INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, INTEGER, NUMERIC) RETURNS NUMERIC AS $$NUMERIC^MAXARGS;
SELECT MAXARGS(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, id) FROM names;

-- Test ERR_FUNCTION_NOT_UNIQUE error in function with lots of parameters for query in https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/816#note_1515900594
CREATE FUNCTION MAXARGS(NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, NUMERIC, INTEGER, INTEGER) RETURNS NUMERIC AS $$INTEGER^MAXARGS;
SELECT MAXARGS(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NULL, id) FROM names;

-- Test https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/816#note_887646106
CREATE FUNCTION SAMEVALUE(INTEGER) RETURNS INTEGER AS $$samevalue1^functions;
CREATE FUNCTION SAMEVALUE(NUMERIC) RETURNS NUMERIC AS $$samevalue2^functions;
CREATE FUNCTION SAMEVALUE(VARCHAR) RETURNS VARCHAR AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(BOOLEAN) RETURNS BOOLEAN AS $$samevalue^functions;
SELECT SAMEVALUE(NULL);	-- expect SAMEVALUE(VARCHAR) to be chosen with no error
DROP FUNCTION IF EXISTS SAMEVALUE(varchar);
SELECT SAMEVALUE(NULL); -- expect ERR_FUNCTION_NOT_UNIQUE error
DROP FUNCTION IF EXISTS SAMEVALUE(numeric);
SELECT SAMEVALUE(NULL); -- expect ERR_FUNCTION_NOT_UNIQUE error
DROP FUNCTION IF EXISTS SAMEVALUE(integer);
SELECT SAMEVALUE(NULL); -- expect SAMEVALUE(BOOLEAN) to be chosen with no error
DROP FUNCTION IF EXISTS samevalue(boolean);
SELECT samevalue(NULL); -- expect ERR_UNKNOWN_FUNCTION error

