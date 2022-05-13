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

SELECT (Products.ProductName || Products.ProductName) FROM Products GROUP BY (Products.ProductName || Products.ProductName) ORDER BY (Products.ProductName || Products.ProductName);

SELECT NOT (Suppliers.Country > 'Denmark'), Suppliers.ContactName, COUNT(ALL Suppliers.City), Suppliers.ContactName FROM Suppliers  CROSS JOIN Categories AS alias1 WHERE (((Suppliers.Address)||(Suppliers.SupplierName))>='Av. das Americanas 12.890Bigfoot Breweries') GROUP BY 1, Suppliers.ContactName ORDER BY COUNT(ALL Suppliers.City), NOT (Suppliers.Country > 'Denmark'), Suppliers.ContactName;
