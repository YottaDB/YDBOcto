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

-- TDTF11 : OCTO288 : Correct handling of 0 month and 0 day dates

SELECT DATE_FORMAT('6507-12-00', '%m %% %m %v %Y %h %a %V %i ');
SELECT DATE_FORMAT('3099-00-28', '%m %p %s %j %w %u %b %D %k %c %% %j %I %d %u %f %k %r %c %p %v %u %e %i %y %v %S %l %l %u %v ');
SELECT DATE_FORMAT('3099-00-28', '%m %% %m %v %Y %h %a %V %i ');
SELECT DATE_FORMAT('2078-00-07', '%S %X %d %y %h ');
SELECT DATE_FORMAT('2005-00-25 -10:10:11', '%x %c %l %% %% %X %I %i %X ');
SELECT DATE_FORMAT('1916-01-00 17:49:14', '%I %m %D %r %X %D %e %m ');
SELECT DATE_FORMAT('2101-00-19 -18:05:23', '%h %w %e %d %X %y %m %d %u %c %W %T %d %k %Y %V %Y %f %Y ');
SELECT DATE_FORMAT('1914-01-00 -13:11:10', '%d %X %i %s ');
SELECT DATE_FORMAT('1949-00-11 11:59:13', '%X %h %S %u %c %k %u %w %% %p %S %T %j %j %p %W %s %D %D %I %W %a %s %m ');
SELECT DATE_FORMAT('2044-00-08 09:41:42', '%v %S %r %% %X %i %j %U %r %U %w %h %U %r %Y %f %% %Y ');
SELECT DATE_FORMAT('2016-01-00', '%X %e %d %y %l %% %x %M %v %x %h %l %i %m %w %x %u %V %Y %H %M %D %j %I %l %D %y %r %l %p %H %r ');
SELECT DATE_FORMAT('2005-00-18 22:18:15', '%y %u %d %c %Y %T %h %H %S %d %W %k %Y %p %S %m %D %f %I %D %h %k %h %k %f %D %v %j %e %l %X %d ');
SELECT DATE_FORMAT('2019-01-00 21:58:45', '%p %m %d %l %j %X %V %s %c %k %r %l %M %d %% %r %s %U %f %j %S ');
SELECT DATE_FORMAT('2044-00-11 -09:32:11', '%l %X %a %Y %U ');
SELECT DATE_FORMAT('2107-00-31', '%X %I %Y %X %h %e %w ');
SELECT DATE_FORMAT('1900-01-00 06:48:28', '%m %l %W %p %e %p %U %d %I %I %I %e %w %j %H %X %D ');
SELECT DATE_FORMAT('2016-00-06 -09:57:31', '%k %v %l %f %m %% %W %h %X %l %S %l %D %w %i %e ');
SELECT DATE_FORMAT('1910-00-29 -04:49:33', '%c %Y %D %I %X %x %T ');
SELECT DATE_FORMAT('1992-01-00', '%v %b %S %l %% %S %b %X %j ');
SELECT DATE_FORMAT('1927-00-19 21:14:01', '%W %v %X %W %r %m %W %c %% ');
SELECT DATE_FORMAT('1955-00-01 -21:39:46', '%% %s %X %k %x %H %d %l %U %a %s %V %c %x %k %d %% %i ');
SELECT DATE_FORMAT('2112-00-30 -17:21:46', '%x %f %l %D %X ');
SELECT DATE_FORMAT('2089-00-20 -22:09:36', '%X %u %S %v %d %h %U %v %w %T %H %f %D %% %Y %l %h %v %m %r %e %m %I %Y %c %c %i ');
