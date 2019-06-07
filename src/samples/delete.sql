#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

DELETE FROM myTable WHERE id = 1;
delete from abc where id in (1, 2, 3, 4);
DELETE FROM abc WHERE abc.value = 5+5/5;
DELETE FROM thing where mixedCase = 1;
