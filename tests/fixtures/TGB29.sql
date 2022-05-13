#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Test whether consecutive executions of literal based GROUP BY queries create different routines based on
--   `group_by_column_num` value or not. If it uses the same routine the output for the second query will be
--   wrong as its executing the first query physical plan (M routine).
-- More details: https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/977#note_953202318
-- Binary expression
SELECT 'test'||'test' FROM names GROUP BY 'test'||'test';
SELECT 'soml'||'test' FROM names GROUP BY 'test'||'test';
-- Unary expression
SELECT +12 FROM names GROUP BY +12;
SELECT +13 FROM names GROUP BY +12;
-- COERCED VALUE TYPE
SELECT 12::INTEGER FROM names GROUP BY 12::INTEGER;
SELECT 13::INTEGER FROM names GROUP BY 12::INTEGER;
-- CASE expression
select CASE 'test' WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END FROM names GROUP BY CASE 'test' WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END;
select CASE 'test' WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 2 END FROM names GROUP BY CASE 'test' WHEN 'Zero' THEN 1 WHEN 'Acid' THEN 2 ELSE 3 END;
-- CALCULATED VALUE TYPE
SELECT nullif('test','test') FROM names GROUP BY nullif('test','test');
SELECT nullif('soml','loms') FROM names GROUP BY nullif('test','test');
