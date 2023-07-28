#################################################################
#								#
# Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TGB07 : OCTO401 : COUNT(DISTINCT) usage issues `Assert failed` error

SELECT COUNT(DISTINCT Country) FROM nwCustomers;

