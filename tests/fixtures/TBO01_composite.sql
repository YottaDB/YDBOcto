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

-- ############################################################################################################
-- YDBOcto#1020: key fixing walks a range from one bound to the other, on the composite table. Its 8 key columns
-- are id0 to id7. The rows, in key order:
--   Name1  (0,1,2,3,4,5,6,7)  Name2  (0,1,2,3,4,5,6,8)  Name3  (0,1,2,3,4,5,7,7)  Name4  (0,1,2,3,4,5,8,7)
--   Name5  (0,1,2,3,4,6,8,7)  Name6  (0,1,2,3,5,6,8,7)  Name7  (0,1,2,4,5,6,8,7)  Name8  (0,1,3,4,5,6,8,7)
--   Name9  (0,2,3,4,5,6,8,7)  Name10 (1,2,3,4,5,6,8,7)
-- [walk 6 to 7 of the cross reference of id6] Name1, Name2 and Name3
SELECT * FROM composite WHERE id6 BETWEEN 6 AND 7;
-- [walk 6 to 7 of the cross reference of id6] LIMIT 2 of the rows in name order: Name1 and Name2
SELECT * FROM composite WHERE id6 BETWEEN 6 AND 7 ORDER BY name LIMIT 2;
-- [walk 6 to 7 of the cross reference of id6] ORDER BY id6: Name1 and Name2 (6), then Name3 (7)
SELECT * FROM composite WHERE id6 BETWEEN 6 AND 7 ORDER BY id6, name;
-- [walk 7 down to 6 of the cross reference of id6] ORDER BY id6 DESC: Name3 (7), then Name1 (6)
SELECT * FROM composite WHERE id6 BETWEEN 6 AND 7 ORDER BY id6 DESC, name LIMIT 2;
-- [walk 0 to 0 of id0] the first key column: the 9 rows with id0 = 0
SELECT * FROM composite WHERE id0 BETWEEN 0 AND 0;
-- [walk 5 to 6 of the cross reference of id5] 6 not included: Name1, Name2, Name3 and Name4
SELECT * FROM composite WHERE id5 >= 5 AND id5 < 6;
-- [walk 7 to 8 of the cross reference of id7] 7 not included: Name2
SELECT * FROM composite WHERE id7 > 7 AND id7 <= 8;
-- [walk 1 to 2 of id1, and 7 to 8 of id6 under it] Name3 to Name10
SELECT * FROM composite WHERE id1 BETWEEN 1 AND 2 AND id6 BETWEEN 7 AND 8;
-- [walk 8 to 6 of the cross reference of id6] no row
SELECT * FROM composite WHERE id6 BETWEEN 8 AND 6;
