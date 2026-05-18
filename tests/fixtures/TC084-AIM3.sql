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
-- AIM-xref path stress test: multiple underlying keys per AIM col_value, with
-- some SKIP'd and some not. The AIM xref for ^skipdupe will have these entries
-- (one per row, third subscript = underlying id):
--   ^aim(0,"#apple",1)  ^aim(0,"#apple",2)
--   ^aim(0,"#cherry",50)  ^aim(0,"#cherry",99)  ^aim(0,"#cherry",100)
--   ^aim(0,"#zebra",200)
-- SKIP '50,99,100,200' on id leaves only the two "apple" rows valid.
--
-- For MAX(name), the outer FOR starts at "#zebra" (largest col_value); inner
-- walks key 200, all SKIP'd, falls through; outer re-orders to "#cherry"; inner
-- walks 50->99->100, all SKIP'd; outer re-orders to "#apple"; inner finds key 1
-- (non-SKIP'd) and exits. Expected MAX = 'apple'.
--
-- This exercises: (1) inner FOR walking past three consecutive SKIP'd keys at
-- the same col_value, (2) outer FOR re-ordering past two col_values whose every
-- underlying key is SKIP'd, and (3) the inner FOR exiting with the key
-- column's LVN reset to "" -- the signal back to the outer FOR that this
-- col_value is unreachable (the outer FOR's QUIT predicate reads that same
-- LVN).
CREATE TABLE skiptest_aim3 (
  id   INTEGER PRIMARY KEY SKIP '50,99,100,200',
  name VARCHAR(64)
) GLOBAL "^skipdupe";
SELECT * FROM skiptest_aim3 ORDER BY id;
SELECT max(name) FROM skiptest_aim3;
SELECT min(name) FROM skiptest_aim3;
