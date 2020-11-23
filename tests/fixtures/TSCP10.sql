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

-- TSCP10 : OCTO628 : RPARENMISSING error when trying to use || operator with hundreds of operands

-- 256 operands in the || operator in the select column list
SELECT
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname
FROM names;

-- 256 code paths in the CASE operator in the select column list
select case id
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1 when 1 then 1
	else 2 end
from names;

-- 256 operands of || using IN operator
SELECT *
FROM names
WHERE	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname
IN (SELECT firstname FROM names);

SELECT *
FROM names
WHERE	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname ||
	firstname || firstname || firstname || firstname || firstname || firstname || firstname || firstname
IN ('Zero', 'Cool');

-- Test tmpl_print_expression with "dot_count" == 0 for various LP_* logical plan types
SELECT * FROM names ORDER BY EXISTS(SELECT * FROM names);
SELECT * FROM names ORDER BY NOT EXISTS(SELECT * FROM names);
SELECT * FROM names ORDER BY (id IS NULL);
SELECT * FROM names ORDER BY id IN (1,2);
SELECT * FROM names ORDER BY id IN (SELECT id FROM names WHERE id % 2 = 0);
SELECT * FROM names ORDER BY id NOT IN (1,2);
SELECT * FROM names ORDER BY id + id;
SELECT * FROM names ORDER BY firstname < lastname;
SELECT * FROM names ORDER BY id::BOOLEAN AND NULL;
SELECT * FROM names ORDER BY id::BOOLEAN OR NULL;
SELECT * FROM names ORDER BY firstname LIKE lastname;
SELECT * FROM names ORDER BY firstname <= ANY (SELECT lastname FROM names WHERE id < 3);
SELECT * FROM names ORDER BY -id;
SELECT * FROM names ORDER BY +id;
SELECT * FROM names ORDER BY NOT id::BOOLEAN;
SELECT * FROM names ORDER BY COALESCE(lastname);
SELECT * FROM names ORDER BY GREATEST(lastname, firstname);
SELECT * FROM names ORDER BY LEAST(lastname, firstname);
SELECT * FROM names ORDER BY NULLIF(lastname, firstname);
-- Note: Below query produces same output in Octo with or without ::NUMERIC but Postgres produces expected output
-- only with ::NUMERIC as otherwise it keeps the type as integer even though the result is not an integer.
SELECT * FROM names ORDER BY 1/(ABS(id - 2*id)::NUMERIC + 1);

