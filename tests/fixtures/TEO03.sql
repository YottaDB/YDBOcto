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

-- Queries that use the "names" schema

SELECT * FROM names WHERE EXISTS (SELECT * FROM names) AND (id < 3 OR id > 3);
SELECT * FROM names WHERE EXISTS (SELECT * FROM names WHERE firstname = 'Zero') AND (id < 3 OR id > 3);
SELECT id FROM names WHERE EXISTS (SELECT COUNT(*) FROM names n2 GROUP BY n2.lastName) AND ((id < 3) OR (id > 3)) ORDER BY id;
SELECT n1.id FROM names n1 INNER JOIN names n2 ON ((n1.firstname > n2.firstname) OR NOT (1 < 2)) WHERE NOT EXISTS (SELECT 1 FROM names WHERE NOT EXISTS (SELECT 1 FROM names));

-- Queries that use the "sqllogic1" schema

SELECT (a+b+c+d+e)/5, d, a+b*2+c*3+d*4 FROM t1
 WHERE EXISTS(SELECT 1 FROM t1 AS x WHERE x.b<t1.b)
   AND d NOT BETWEEN 110 AND 150
 ORDER BY 3,2,1
;

-- Queries that use the "northwind" schema

SELECT DISTINCT Categories.CategoryID, (SELECT alias1.ShipperID FROM Shippers alias1 ORDER BY alias1.ShipperID LIMIT 1) AS alias1, Categories.CategoryName FROM Categories INNER JOIN Products AS alias2 ON ((Categories.CategoryName > alias2.Unit) OR NOT ((Categories.CategoryID <= alias2.ProductID)) AND (('Dried fruit and bean curd' < '48 - 6 oz jars')) OR (Categories.CategoryID = SOME (SELECT ALL alias2.CategoryID FROM Products alias2 WHERE alias2.ProductName BETWEEN 'Louisiana Hot Spiced Okra' AND 'Escargots de Bourgogne' ORDER BY alias2.CategoryID LIMIT 1)) AND (Categories.Description > '48 - 6 oz jars')) WHERE NOT EXISTS (SELECT MIN(alias4.SupplierID), alias4.City FROM Suppliers alias4 GROUP BY alias4.City HAVING NOT EXISTS (SELECT ALL alias5.ShipperID, alias5.OrderDate FROM Orders alias5));

