#################################################################
#								#
# Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- This is invoked from Stage 1 of the TCF023 or TDF002 subtest

-- TCF023 : OCTO90 : Rerunning query after CREATE FUNCTION should recreate plans that relied on the recreated function
-- TDF002 : OCTO90 : DROP FUNCTION should delete db nodes for plans that relied on the dropped function

SELECT 1 from names where ABSF(id) = 2 AND REPLACEF('Zero','a','b') = 'Zero';
SELECT 2 from names where ABSF(id) != 2 AND REPLACEF('Zero','a','b') = 'Zero';
SELECT 3 from names where ABSF(id) = 2 AND REPLACEF('Zero','a','b') != 'Zero';
SELECT 4 from names where ABSF(id) != 2 AND REPLACEF('Zero','a','b') != 'Zero';
SELECT 5 from names where ABSF(id) != 2;
SELECT 6 from names where ABSF(id) = 2;
SELECT 7 from names where REPLACEF('Zero','a','b') != 'Zero';
SELECT 8 from names where REPLACEF('Zero','a','b') = 'Zero';

