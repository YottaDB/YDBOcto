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

-- TWI10 : OCTO424 : Floating point numeric literals greater than 10 get incorrectly treated as Table References

SELECT * FROM Products WHERE Products.Price IN (19, 15, 32, 7.75, 19.45, 32, 16.25)

