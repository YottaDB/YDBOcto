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

-- TDTF06 : OCTO288 : No stack smashing in DATE_FORMAT() if input date string is too long
SELECT DATE_FORMAT('123456789123456789-00-00 -25:30:30', '%%x:%x');
SELECT DATE_FORMAT('1900-123456789123456789-00 -25:30:30', '%%x:%x');
SELECT DATE_FORMAT('1900-00-123456789123456789 -25:30:30', '%%x:%x');
SELECT DATE_FORMAT('1900-00-00 -123456789123456789:30:30', '%%x:%x');
SELECT DATE_FORMAT('1900-00-00 -25:123456789123456789:30', '%%x:%x');
SELECT DATE_FORMAT('1900-00-00 -25:30:123456789123456789', '%%x:%x');
