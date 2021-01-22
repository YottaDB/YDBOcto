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

-- TCF009 : OCTO345 : % as leading character in M labels and routine names

CREATE FUNCTION TCF009() RETURNS VARCHAR AS $$%TCF009^%TESTPERCENT;
select TCF009() from names;
DROP FUNCTION TCF009();

CREATE FUNCTION TCF009() RETURNS VARCHAR AS $$%^%;
select TCF009() from names;
DROP FUNCTION TCF009();

