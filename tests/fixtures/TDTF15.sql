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

-- TDTF15 : OCTO288 : Correct handling of %x format

SELECT DATE_FORMAT('1958-00-25 18:57:55', '%x');
SELECT DATE_FORMAT('2082-00-02', '%x');
SELECT DATE_FORMAT('2063-00-16', '%x');
