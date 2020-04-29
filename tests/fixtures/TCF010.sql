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

-- TCF010 : OCTO345 : CREATE FUNCTION with various return types

-- Return type of BOOLEAN
CREATE FUNCTION RETTRUE() RETURNS BOOLEAN AS $$RETTRUE^TCF010;
CREATE FUNCTION RETFALSE() RETURNS BOOLEAN AS $$RETFALSE^TCF010;
SELECT RETTRUE() || id::text FROM names;
SELECT id FROM names WHERE RETTRUE();
SELECT lastname FROM names WHERE RETFALSE();

CREATE FUNCTION RETTRUESTR() RETURNS BOOLEAN AS $$RETTRUESTR^TCF010;
CREATE FUNCTION RETFALSESTR() RETURNS BOOLEAN AS $$RETFALSESTR^TCF010;
SELECT RETTRUESTR() || id::text FROM names;
SELECT id FROM names WHERE RETTRUESTR();
SELECT lastname FROM names WHERE RETFALSESTR();

-- Return type of INTEGER
CREATE FUNCTION RETINT(INTEGER) RETURNS INTEGER AS $$RETINT^TCF010;
SELECT * FROM names WHERE id = RETINT(1) OR id = RETINT(2);
SELECT * FROM names WHERE id = (RETINT(1)+RETINT(2));

-- Return type of NUMERIC
CREATE FUNCTION RETNUM(NUMERIC) RETURNS NUMERIC AS $$RETINT^TCF010;
SELECT RETNUM(3.14) FROM names;
SELECT * FROM names WHERE id = (RETNUM(2.5) + RETNUM(2.5));
SELECT * FROM names WHERE id = RETNUM(3.14)::integer;

-- Return type of VARCHAR
CREATE FUNCTION RETVARCHAR2(NUMERIC, VARCHAR) RETURNS VARCHAR AS $$RETVARCHAR2^TCF010;
SELECT RETVARCHAR2(3.14, "... is pi") FROM names;
CREATE FUNCTION RETVARCHAR(VARCHAR) RETURNS VARCHAR AS $$RETVARCHAR^TCF010;
SELECT * FROM names WHERE firstname = RETVARCHAR(firstname);
SELECT firstname FROM names WHERE firstname = (RETVARCHAR("Ze") || RETVARCHAR("ro")) OR lastname = (RETVARCHAR("B") || RETVARCHAR("u") ||RETVARCHAR("rn"));

