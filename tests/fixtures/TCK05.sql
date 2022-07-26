#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCK05 : OCTO877 : Incorrect results if WHERE clause references multiple columns of composite table

SELECT ALL alias2.id3 FROM composite  INNER JOIN composite AS alias1 ON ((composite.name != alias1.name) OR (4 <= NULL)) INNER JOIN composite AS alias2 ON (((composite.id2 = alias2.id3)) AND ((composite.name = alias2.name))) WHERE composite.id6 BETWEEN 6 AND 8 ORDER BY alias2.id3 LIMIT 28;

