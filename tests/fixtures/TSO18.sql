#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSO18 : OCTO981 : Test VALUES with SET operations in sub-query when used with WHERE and OR in outer query
select * from (SELECT * FROM (VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', NULL), (5, 'Zero', 'Cool'))x where column1 < 4 UNION VALUES (0, 'Zero', 'Cool'), (1, 'Acid', 'Burn'), (2, 'Cereal', 'Killer'), (3, 'Lord', 'Nikon'), (4, 'Joey', NULL), (5, 'Zero', 'Cool')) n1 where true  OR false;
select * from (VALUES (0) UNION ALL VALUES (0))d where true or false;
select * from (select 1 INTERSECT (values(1)))x where true OR false;
select * from (select 1 except values(1)) n1 where true or false;
