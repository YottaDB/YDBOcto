#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

SELECT ALL nullnamesb.id, alias3.* FROM nullnamesb  RIGHT JOIN (SELECT ALL alias1.yearsTenured, alias1.firstName FROM nullnames alias1 ORDER BY alias1.firstName, alias1.yearsTenured) AS alias1 ON ((nullnamesb.yearsTenured <= alias1.yearsTenured) OR (nullnamesb.id = alias1.yearsTenured)) LEFT JOIN (SELECT DISTINCT alias3.lastName FROM nullnamesb alias3 ORDER BY alias3.lastName) AS alias3 ON (((nullnamesb.lastName != alias3.lastName)) OR NOT (nullnamesb.salary <= ANY (SELECT DISTINCT alias4.salary FROM nullnames alias4 ORDER BY alias4.salary LIMIT 1))) WHERE nullnamesb.exempt = FALSE ORDER BY alias3.*, nullnamesb.id;
