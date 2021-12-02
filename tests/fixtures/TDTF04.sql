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

-- TDTF04 : OCTO288 : Support dates with 0 for month and/or date in date_format() MySQL function

SELECT DATE_FORMAT('1999-01-01', '%X %V');
SELECT DATE_FORMAT('2006-06-00', '%d');
SELECT DATE_FORMAT('1999-00-01', '%X %V');
SELECT DATE_FORMAT('0000-06-00', '%d');
SELECT DATE_FORMAT('0000-00-00', '%d');

SELECT DATE_FORMAT('0-0-0 0:0:0', '%%U:%U %%u:%u %%V:%V %%v:%v %%X:%X %%x:%x');
SELECT DATE_FORMAT('1999-01-01', '%%U:%U %%u:%u %%V:%V %%v:%v %%X:%X %%x:%x');
SELECT DATE_FORMAT('2006-06-00', '%%U:%U %%u:%u %%V:%V %%v:%v %%X:%X %%x:%x');
SELECT DATE_FORMAT('1999-00-01', '%%U:%U %%u:%u %%V:%V %%v:%v %%X:%X %%x:%x');
SELECT DATE_FORMAT('0000-06-00', '%%U:%U %%u:%u %%V:%V %%v:%v %%X:%X %%x:%x');
SELECT DATE_FORMAT('0000-00-00', '%%U:%U %%u:%u %%V:%V %%v:%v %%X:%X %%x:%x');

SELECT DATE_FORMAT('6120-2-26', '%d');
SELECT DATE_FORMAT('4112-11-18', '%X %V');
