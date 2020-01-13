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

-- TSO11 : OCTO273/OCTO326/OCTO327/OCTO328

(select * from names UNION select * from names) UNION ALL (select * from names UNION select * from names);
(select * from names where id > 1 or id = 0) EXCEPT (select * from names where id < 4 or id = 5);
(select * from names union all select * from names) INTERSECT (select * from names where id < 4);
(select * from (select * from names union select * from names)) union (select * from names);

