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

-- TDTF13 : OCTO288 : Correct handling of %j format

SELECT DATE_FORMAT('2037-07-00', '%a %S %s %d %S %j ');
SELECT DATE_FORMAT('2141-00-31 23:12:15', '%r %V %T %j %Y %Y %W %H %j %c %c %l %U ');
SELECT DATE_FORMAT('1985-00-16', '%H %j %H %I %H %l %c %Y %w %W %H %j %u %k %% %s %u %m %d %r %w %T %i ');
SELECT DATE_FORMAT('1997-05-00', '%d %b %x %a %U %e %j %j %l %f %y %S %s %b %Y ');
SELECT DATE_FORMAT('2133-00-26', '%p %x %i %i %j %y %p %H %i %s %Y %% %w %a %l %x %H %h %k %S %w %D %l %c %x ');
