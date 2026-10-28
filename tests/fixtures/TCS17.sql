#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCS17 : A bare literal like 't' in a CASE argument takes the type another argument resolves the CASE to

-- These queries used to return an empty value, as 't' etc. stayed a VARCHAR in a BOOLEAN CASE
-- Literal in a THEN argument, type resolved by the ELSE argument
SELECT CASE WHEN true THEN 't' ELSE (1=1) END;
SELECT CASE WHEN true THEN 'f' ELSE (1=1) END IS NULL;
SELECT CASE WHEN true THEN '1' ELSE (1=0) END AND true;
-- Literal in the ELSE argument, type resolved by a THEN argument
SELECT CASE WHEN false THEN (1=1) ELSE 'f' END;
SELECT CASE WHEN firstname = 'Zero' THEN id = 0 ELSE 'no' END FROM names;
-- Literal in a later THEN argument, type resolved by an earlier THEN argument
SELECT CASE WHEN false THEN (1=1) WHEN true THEN 'no' END;
-- Literals in earlier THEN arguments, type resolved by a later THEN argument
SELECT CASE WHEN false THEN 'yes' WHEN false THEN NULL WHEN true THEN 'n' WHEN false THEN (1=0) END;
-- Simple CASE form
SELECT CASE 1 WHEN 1 THEN 't' ELSE (1=0) END;
-- Nested CASE
SELECT CASE WHEN true THEN (CASE WHEN true THEN 't' ELSE (1=0) END) ELSE false END;

-- These queries already worked, as their CASE type is VARCHAR
SELECT CASE WHEN firstname = 'Zero' THEN 't' ELSE firstname END FROM names;
SELECT CASE WHEN false THEN firstname ELSE 'y' END FROM names;
SELECT CASE WHEN true THEN 't' ELSE 'f' END;
SELECT CASE WHEN true THEN 't' END;
SELECT CASE WHEN true THEN 't' ELSE NULL END;
