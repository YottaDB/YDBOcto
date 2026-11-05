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

-- The cross reference key of the VARCHAR key k: order_by_xref_walk, key_column_xref, desc
select k as kk from TOB25a order by k desc limit 1;
-- The cross reference key of the INTEGER key y, the second: order_by_xref_walk and key_column_xref, no desc
select x as xx, y from TOB25b order by y, x limit 1;
-- The cross reference key of the READONLY DATE key k2: order_by_xref_walk, but not key_column_xref, as it holds
-- converted values. The key of k2 itself follows it.
select k2 as kk2 from TOB25d order by k2 limit 1;
