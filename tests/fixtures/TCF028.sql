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

-- TCF028 : OCTO288 : CREATE FUNCTION works with case-insensitive parenless functions, both with and without parentheses

SELECT CURRENT_CATALOG;
SELECT CURRENT_ROLE;
SELECT CURRENT_USER;
SELECT CURRENT_SCHEMA;
SELECT CURRENT_TIME;
SELECT CURRENT_TIMESTAMP;
SELECT LOCALTIME;
SELECT LOCALTIMESTAMP;
SELECT SESSION_USER;

SELECT CURRENT_CATALOG();
SELECT CURRENT_ROLE();
SELECT CURRENT_USER();
SELECT CURRENT_SCHEMA();
SELECT CURRENT_TIME();
SELECT CURRENT_TIMESTAMP();
SELECT LOCALTIME();
SELECT LOCALTIMESTAMP();
SELECT SESSION_USER();

SELECT CuRrEnT_cAtAlOg;
SELECT CuRrEnT_rOlE;
SELECT CuRrEnT_uSeR;
SELECT CuRrEnT_sChEmA;
SELECT CuRrEnT_tImE;
SELECT CuRrEnT_tImEsTaMp;
SELECT LoCaLtImE;
SELECT LoCaLtImEsTaMp;
SELECT SeSsIoN_UsEr;
