#################################################################
#								#
# Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSS14 : OCTO473 : Incorrect results when ON clause of a JOIN uses column references from a sub-query

SELECT CustomerID,EmployeeID,ContactName,Notes FROM nwCustomers INNER JOIN (SELECT EmployeeID,Notes FROM Employees) AS alias4 ON (nwCustomers.ContactName > alias4.Notes) ORDER BY CustomerID,EmployeeID;

