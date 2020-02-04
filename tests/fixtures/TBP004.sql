#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

UPDATE myTable
SET id = 3, name = "Orion"
WHERE breed = "Black Lab";
UPDATE myTable
SET name = (SELECT name FROM tableOfDogs WHERE breed = "Black lab");
