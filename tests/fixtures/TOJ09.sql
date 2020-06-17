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

-- TOJ09 : OCTO534 : northwind schema : Incorrect query results when LEFT/RIGHT/FULL JOIN is used with OR in WHERE clause
SELECT DISTINCT alias1.Address, Shippers.ShipperName, alias1.SupplierName FROM Shippers  LEFT JOIN Suppliers AS alias1 ON ((Shippers.Phone = alias1.City) AND ((Shippers.ShipperName = '031-987 65 43'))) WHERE (('(503) 555-9831' > Shippers.Phone) OR ((Shippers.ShipperID >= 1)) OR NOT (2 != Shippers.ShipperID));

