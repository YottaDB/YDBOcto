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

-- TDTF14 : OCTO288 : Correct handling of %U and %M formats

SELECT DATE_FORMAT('1926-00-01', '%U ');
SELECT DATE_FORMAT('2062-00-18', '%d %U %k %h ');
SELECT DATE_FORMAT('2057-00-13', '%r %H %% %w %M ');
