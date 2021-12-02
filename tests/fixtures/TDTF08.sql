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

-- TDTF08 : OCTO288 : Correct handling of extra hyphens in date string

SELECT DATE_FORMAT('----1900-00-00 12:30:30', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-----1-00 12:30:30', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-----1 12:30:30', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-00 ----12:30:30', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-00 12:----30:30', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-00 12:30:----30', '%Y-%m-%d %H:%i:%s');
