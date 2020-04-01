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

-- TCS12 : OCTO460 : CASE in SELECT column list causes assertion failed in lp_replace_derived_table_references.c

SELECT CASE WHEN (1=1) THEN 1 ELSE 3 END FROM names n1 INNER JOIN (SELECT n2.id FROM names n2) n3 ON n1.id = n3.id;

