#################################################################
#								#
# Copyright (c) 2023-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TITER08 : ITERATOR key-fixing is per-column. VIRTUAL columns fall back to nested-loop;
-- non-VIRTUAL columns are still fixed. The CREATE TABLE itself is in TITER08_create.sql
-- which is loaded silently so the LP dump captured here is only the two JOIN plans.
-- FULL JOIN on id: id is VIRTUAL -> guard fires -> nested-loop full enumeration (LP_KEY_ADVANCE on id).
SELECT * FROM cat_rec_titles n1 FULL JOIN cat_rec_titles n2 ON n1.id = n2.id;
-- INNER JOIN on catsys: catsys is non-VIRTUAL at key_num=0 -> key-fix applies (LP_KEY_FIX on n2.catsys).
SELECT * FROM cat_rec_titles n1 INNER JOIN cat_rec_titles n2 ON n1.catsys = n2.catsys;
