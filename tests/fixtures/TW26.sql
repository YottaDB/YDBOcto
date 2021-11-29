#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TW26 : OCTO790 : Octo incorrectly issues ERR_SUBQUERY_MULTIPLE_ROWS error
-- Below are queries pasted from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/790#description.
-- Without the code fixes for YDBOcto#790, the 3rd query below used to incorrectly issue an ERR_SUBQUERY_MULTIPLE_ROWS error.

SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE a.id = (b.id + 2));
SELECT * FROM names a WHERE a.firstName != (SELECT NULL from names limit 2);
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE a.id = (b.id + 2));
SELECT * FROM names a WHERE a.firstName != (SELECT b.firstName FROM names b WHERE a.id = (b.id + 2));

