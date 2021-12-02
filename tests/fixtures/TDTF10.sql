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

-- TDTF10 : OCTO288 : Correct handling of dates in the year 1900

SELECT DATE_FORMAT('1900-01-25 03:01:44', '%a %w %% %p ');
SELECT DATE_FORMAT('1900-10-17', '%d %j %w %i %c ');
SELECT DATE_FORMAT('1900-03-13', '%v %T %i %d %h %y %b %p %h %e %k %d %j %V %d %v %I %x %e %W %e %y %d %k %U ');
SELECT DATE_FORMAT('1900-07-27 00:20:27', '%V %f %p %d %b %p %s %j %k %M %s %r %w %w %i %T %V %X %D %d %k %% %i %m %W %k %k %m ');
SELECT DATE_FORMAT('1900-09-25 00:33:17', '%v %k %S %j %r %S %p %e %u %a %l %S %m %I ');
SELECT DATE_FORMAT('1900-09-05 02:30:10', '%I %l %V %m %V %r %j %D %U %% %b %X %y %X %T %W %M ');
