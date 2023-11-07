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

-- TOB21 : OCTO1021 : Test that ORDER BY COLUMN NUM when used with LIMIT returns correct results

SELECT id+10,firstname FROM names ORDER BY firstname LIMIT 4;
SELECT id+10,firstname FROM names ORDER BY 2 LIMIT 4;

-- The below queries are similar to the above in that they use GROUP BY (instead of ORDER BY above).
-- They did not suffer from the YDBOcto#1021 issue because GROUP BY COLUMN NAME exercises a slightly
-- different code path than ORDER BY COLUMN NAME (in "src/qualify_statement.c") but are included here
-- just in case that changes in the future.
SELECT 10,firstname FROM names GROUP BY firstname ORDER BY firstname LIMIT 4;
SELECT 10,firstname FROM names GROUP BY firstname ORDER BY 2         LIMIT 4;
SELECT 10,firstname FROM names GROUP BY 2         ORDER BY firstname LIMIT 4;
SELECT 10,firstname FROM names GROUP BY 2         ORDER BY 2         LIMIT 4;

