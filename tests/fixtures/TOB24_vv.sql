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

-- TOB24 : OCTO1160 : -vv shows which LP_KEY walks an ORDER BY column ("order_by_xref_walk: true") and which LP_KEY walks
-- in descending order ("desc: true"). Each query selects "id, lastname", which no other TOB24 query does, so that it gets
-- a new plan whose logical plan -vv shows.

-- Cross reference walk of lastname, ascending and descending
select id, lastname from names order by lastname, id limit 1;
select id, lastname from names order by lastname desc, id desc limit 1;
-- Primary key FOR loop in descending order (YDBOcto#959)
select id, lastname from names order by id desc limit 1;
-- Cross reference that the WHERE fixes to a range, walked in descending order
select id, lastname from names where lastname > 'C' order by lastname desc, id;
-- Not optimized: no key shows either line
select id, lastname from names order by lastname, firstname limit 1;
