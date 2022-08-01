#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCS14 : OCTO544 : Assertion failure and Errors when IN is used in SELECT column list

-- This file contains queries that run fine in Octo but error out or produce different output in Postgres
-- due to false/true being 'f'/'t' in Postgres vs 0/1 in Octo. Hence they cannot be in TCS14.sql (which goes
-- through the crosscheck function and expects Octo and Postgres output to be identical).

-- Below queries used to assert fail due to no column alias defined for a CASE statement column in SELECT column list
SELECT CASE 1 in (1) when 'g' then 2 end;
SELECT CASE 1 in (1) when 1 then 2 end;
SELECT CASE 1 in (1) when 3 then 2 end;
