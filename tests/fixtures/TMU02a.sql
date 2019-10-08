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

# SELECT * FROM names;
# SELECT * FROM names where firstname = 'Acid';
# SELECT * FROM names where firstname = 'Acid' AND lastname = 'Burn';
# SELECT * FROM names where firstname = 'Lord' OR id = 5;
# SELECT id FROM names where 1 = 1;

# SELECT * from names n1 LEFT JOIN names n2 ON n1.id = n2.id;
# SELECT * from names n1 LEFT JOIN names n2 ON n1.id = n2.id WHERE n1.id = 2;
# SELECT * from names n1 INNER JOIN names n2 ON n1.id = n2.id WHERE (n1.id = 2) OR (n2.firstname = 'Zero');
# SELECT * from names n1 INNER JOIN names n2 ON n1.id = n2.id WHERE (n1.firstname = 'Joey') OR (n2.lastname = 'Nikon');
# SELECT n1.firstname from names n1 RIGHT JOIN names n2 ON n1.id = n2.id WHERE (n1.id = 0);

SELECT * FROM Customers WHERE CustomerID = 1;
SELECT * FROM OrderDetails WHERE OrderDetailID = 1;
SELECT * FROM Orders WHERE OrderID = 10443;
SELECT * FROM Suppliers WHERE SupplierID = 1;
SELECT * FROM Products WHERE ProductID = 1;
SELECT * FROM Categories WHERE CategoryID = 1;
SELECT * FROM Employees WHERE EmployeeID = 1;
SELECT * FROM Shippers WHERE ShipperID = 1;
SELECT * FROM OrderDetails WHERE OrderDetailID = 2;
SELECT * FROM OrderDetails WHERE OrderDetailID = 3;
