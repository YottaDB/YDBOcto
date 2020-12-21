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

SELECT ALL alias1.id FROM names  RIGHT OUTER JOIN names AS alias1 ON (((names.firstName <= alias1.firstName))) RIGHT OUTER JOIN names AS alias2 ON (((names.id != ALL (SELECT DISTINCT alias2.id FROM names alias2 WHERE (NOT (1 < alias2.id) OR NOT (alias2.id >= 3)) ORDER BY alias2.id LIMIT 1)) AND (names.firstName <= alias2.lastName) OR NOT (names.firstName >= alias2.firstName))) LIMIT 6; -- rowcount-only-check
