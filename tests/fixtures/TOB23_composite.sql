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

-- TOB23 : OCTO1161 : ORDER BY on a key column is optimized when the key columns before it are fixed to one value

-- Leading key columns fixed with "=". ORDER BY optimization takes effect.
select * from composite where id0 = 0 order by id1 desc, id2 desc, id3 desc, id4 desc, id5 desc, id6 desc, id7 desc limit 3;
select * from composite where id0 = 0 and id1 = 1 and id2 = 2 and id3 = 3 and id4 = 4 order by id5, id6, id7 desc;
select * from composite where id1 = 1 and id0 = 0 order by id2 desc, id3 desc, id4 desc, id5 desc, id6 desc, id7 desc limit 2;

-- The fixed key column is itself an ORDER BY column. ORDER BY optimization takes effect.
select * from composite where id0 = 0 order by id0 desc, id1 desc, id2, id3, id4, id5, id6, id7 limit 4;

-- ORDER BY optimization does not take effect
-- 1) The ORDER BY skips a key column that is not fixed (id1)
select * from composite where id0 = 0 order by id2, id1, id3, id4, id5, id6, id7;
-- 2) The leading key column is fixed to a LIST of values (IN) or a range of values
select * from composite where id0 in (0, 1) order by id1 desc, id2 desc, id3 desc, id4 desc, id5 desc, id6 desc, id7 desc, id0;
select * from composite where id0 >= 0 order by id1 desc, id2 desc, id3 desc, id4 desc, id5 desc, id6 desc, id7 desc, id0;
-- 3) The fixed key column is an ORDER BY column, but after the ORDER BY column of the next key (id1)
select * from composite where id0 = 0 order by id1, id0, id2 desc, id3 desc, id4 desc, id5 desc, id6 desc, id7 desc limit 4;
