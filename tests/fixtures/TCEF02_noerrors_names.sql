#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
SELECT NULLIF('(none)', '(none)');
SELECT NULLIF('a', '(none)');
SELECT NULLIF(NULL, '(none)');
SELECT NULLIF(NULL, NULL);
SELECT NULLIF(1, 2);
SELECT NULLIF(1, 1);
SELECT NULLIF(1, 1.0);
SELECT NULLIF(1.0, 1);
