#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TDTF03 : OCTO288 : Calling DATE_FORMAT() when emulating PostgreSQL issues an error

SELECT DATE_FORMAT('2000-01-01 12:00:00', '%a %b %D %Y');
