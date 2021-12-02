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

-- TDTF09 : OCTO288 : Correct handling of non-numeric characters between date/time fields
SELECT DATE_FORMAT('1900-00-00 a12:30:40', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-00 12:s:30:40', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-00 12:30:40', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-s00-00 12:30:40', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-00 12:30:-:@:..(:30', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-00 12:30:-:@:2..(:30', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-00 12:30:[:30', '%Y-%m-%d %H:%i:%s');
SELECT DATE_FORMAT('1900-00-00 12s:30:40', '%Y-%m-%d %H:%i:%s');
