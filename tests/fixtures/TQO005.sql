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

-- TQO005 : #1141 : The bare and the qualified form of each comparison operator select the exact same rows
-- Each query below is the symmetric difference of the bare form and the qualified form of one operator.
-- All of them are expected to return 0 rows.

(select id from names where id = 3 except select id from names where id operator(pg_catalog.=) 3)
  union (select id from names where id operator(pg_catalog.=) 3 except select id from names where id = 3);

(select id from names where id <> 3 except select id from names where id operator(pg_catalog.<>) 3)
  union (select id from names where id operator(pg_catalog.<>) 3 except select id from names where id <> 3);

(select id from names where id < 3 except select id from names where id operator(pg_catalog.<) 3)
  union (select id from names where id operator(pg_catalog.<) 3 except select id from names where id < 3);

(select id from names where id > 3 except select id from names where id operator(pg_catalog.>) 3)
  union (select id from names where id operator(pg_catalog.>) 3 except select id from names where id > 3);

(select id from names where id <= 3 except select id from names where id operator(pg_catalog.<=) 3)
  union (select id from names where id operator(pg_catalog.<=) 3 except select id from names where id <= 3);

(select id from names where id >= 3 except select id from names where id operator(pg_catalog.>=) 3)
  union (select id from names where id operator(pg_catalog.>=) 3 except select id from names where id >= 3);
