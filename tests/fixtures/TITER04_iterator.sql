#################################################################
#								#
# Copyright (c) 2023-2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TITER04 : ITERATOR Disables Cross References/ITERATOR JOINS
SELECT * from names_iterator;
SELECT * from names_iterator where firstName = 'Zero';
SELECT * FROM names_iterator n1 FULL JOIN names n2 ON n2.id = n1.id;
