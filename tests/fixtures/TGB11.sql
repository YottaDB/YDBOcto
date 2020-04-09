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

-- TGB11 : OCTO452 : Assert failure when invalid column name is specified in GROUP BY

-- Test simple case of GROUP BY column list having an unknown column
SELECT 1 FROM names GROUP BY unknowncolumn;

-- Test case WHERE GROUP BY column list has an unknown column and a known column FROM a parent query
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY n1.id,unknowncolumn);
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY unknowncolumn,n1.id);

-- Slight variations of above tests
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY n1.unknowncolumn);
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY n2.id,unknowncolumn);
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY unknowncolumn,n2.id);
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY n2.id,n1.id,unknowncolumn);
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY n2.id,unknowncolumn,n1.id);
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY unknowncolumn,n1.id,n2.id);
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY unknowncolumn,n2.id,n1.id);
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY n1.id,n2.id,unknowncolumn);
SELECT * FROM names n1 WHERE n1.id = (SELECT 1 FROM names n2 GROUP BY n1.id,unknowncolumn,n2.id);

