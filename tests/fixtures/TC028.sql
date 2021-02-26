#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC028 : OCTO527 : Correct precedence of NOT NULL column specifications

SELECT * FROM nullcharnames n1 LEFT JOIN nullcharnames n2 ON n1.lastname = n2.lastname;

SELECT * FROM nullcharnames WHERE firstname = '' OR lastname IS NULL;	-- rowcount-only-check

SELECT * FROM nullcharnames n1 NATURAL JOIN nullcharnames n2;

SELECT * FROM nullcharnames n1 LEFT JOIN nullcharnames n2 ON n1.firstname = n2.firstname;
