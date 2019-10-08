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

# SELECT * from names;
# SELECT * FROM names where firstname = 'Acid';
# SELECT * FROM names where firstname = 'Acid' AND lastname = 'Burn';
# SELECT * FROM names where firstname = 'Lord' OR id = 5;
# SELECT id FROM names where 1 = 1;

# SELECT * from names n1 LEFT JOIN names n2 ON n1.id = n2.id;
# SELECT * from names n1 LEFT JOIN names n2 ON n1.id = n2.id WHERE n1.id = 2;
# SELECT * from names n1 INNER JOIN names n2 ON n1.id = n2.id WHERE (n1.id = 2) OR (n2.firstname = 'Zero');
# SELECT * from names n1 INNER JOIN names n2 ON n1.id = n2.id WHERE (n1.firstname = 'Joey') OR (n2.lastname = 'Nikon');
# SELECT n1.firstname from names n1 RIGHT JOIN names n2 ON n1.id = n2.id WHERE (n1.id = 0);

# SELECT * FROM (SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names UNION SELECT * FROM names);
# SELECT * FROM (SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names);
# SELECT * FROM (SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = 1);
# SELECT * FROM (SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
# SELECT * FROM (SELECT * FROM names) n1 WHERE id = ANY (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
# SELECT * FROM (SELECT * FROM names) n1 WHERE id = ALL (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');

# SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names UNION SELECT * FROM names);
# SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names);
# SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = 1);
# SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE EXISTS (SELECT * FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
# SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE id = ANY (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');
# SELECT * FROM (SELECT * FROM names UNION ALL SELECT * FROM names) n1 WHERE id = ALL (SELECT n2.id FROM names n2 WHERE n2.id = 1 OR n2.firstname = 'Zero');

SELECT * FROM Customers WHERE CustomerID = 1;
SELECT * FROM OrderDetails WHERE OrderDetailID = 1;
SELECT * FROM Orders WHERE OrderID = 10443;
SELECT * FROM Suppliers WHERE SupplierID = 1;
SELECT * FROM Products WHERE ProductID = 1;
SELECT * FROM Categories WHERE CategoryID = 1;
SELECT * FROM Employees WHERE EmployeeID = 1;
SELECT * FROM Shippers WHERE ShipperID = 1;
SELECT * FROM OrderDetails WHERE OrderDetailID = 10;
SELECT * FROM OrderDetails WHERE OrderDetailID = 11;
SELECT * FROM OrderDetails WHERE OrderDetailID = 12;
SELECT * FROM OrderDetails WHERE OrderDetailID = 13;
SELECT * FROM OrderDetails WHERE OrderDetailID = 14;
SELECT * FROM OrderDetails WHERE OrderDetailID = 15;
SELECT * FROM OrderDetails WHERE OrderDetailID = 16;
SELECT * FROM OrderDetails WHERE OrderDetailID = 17;
SELECT * FROM OrderDetails WHERE OrderDetailID = 18;
SELECT * FROM OrderDetails WHERE OrderDetailID = 19;
SELECT * FROM OrderDetails WHERE OrderDetailID = 20;
SELECT * FROM OrderDetails WHERE OrderDetailID = 21;
SELECT * FROM OrderDetails WHERE OrderDetailID = 22;
SELECT * FROM OrderDetails WHERE OrderDetailID = 23;
SELECT * FROM OrderDetails WHERE OrderDetailID = 24;
SELECT * FROM OrderDetails WHERE OrderDetailID = 25;
SELECT * FROM OrderDetails WHERE OrderDetailID = 26;
SELECT * FROM OrderDetails WHERE OrderDetailID = 27;
SELECT * FROM OrderDetails WHERE OrderDetailID = 28;
SELECT * FROM OrderDetails WHERE OrderDetailID = 29;
SELECT * FROM OrderDetails WHERE OrderDetailID = 30;
SELECT * FROM OrderDetails WHERE OrderDetailID = 31;
SELECT * FROM OrderDetails WHERE OrderDetailID = 32;
SELECT * FROM OrderDetails WHERE OrderDetailID = 33;
SELECT * FROM OrderDetails WHERE OrderDetailID = 34;
SELECT * FROM OrderDetails WHERE OrderDetailID = 35;
SELECT * FROM OrderDetails WHERE OrderDetailID = 36;
SELECT * FROM OrderDetails WHERE OrderDetailID = 37;
SELECT * FROM OrderDetails WHERE OrderDetailID = 38;
SELECT * FROM OrderDetails WHERE OrderDetailID = 39;
SELECT * FROM OrderDetails WHERE OrderDetailID = 40;
SELECT * FROM OrderDetails WHERE OrderDetailID = 41;
SELECT * FROM OrderDetails WHERE OrderDetailID = 42;
SELECT * FROM OrderDetails WHERE OrderDetailID = 43;
SELECT * FROM OrderDetails WHERE OrderDetailID = 44;
SELECT * FROM OrderDetails WHERE OrderDetailID = 45;
SELECT * FROM OrderDetails WHERE OrderDetailID = 46;
SELECT * FROM OrderDetails WHERE OrderDetailID = 47;
SELECT * FROM OrderDetails WHERE OrderDetailID = 48;
SELECT * FROM OrderDetails WHERE OrderDetailID = 49;
SELECT * FROM OrderDetails WHERE OrderDetailID = 50;
SELECT * FROM OrderDetails WHERE OrderDetailID = 51;
SELECT * FROM OrderDetails WHERE OrderDetailID = 52;
SELECT * FROM OrderDetails WHERE OrderDetailID = 53;
SELECT * FROM OrderDetails WHERE OrderDetailID = 54;
SELECT * FROM OrderDetails WHERE OrderDetailID = 55;
SELECT * FROM OrderDetails WHERE OrderDetailID = 56;
SELECT * FROM OrderDetails WHERE OrderDetailID = 57;
SELECT * FROM OrderDetails WHERE OrderDetailID = 58;
SELECT * FROM OrderDetails WHERE OrderDetailID = 59;
SELECT * FROM OrderDetails WHERE OrderDetailID = 60;
SELECT * FROM OrderDetails WHERE OrderDetailID = 61;
SELECT * FROM OrderDetails WHERE OrderDetailID = 62;
SELECT * FROM OrderDetails WHERE OrderDetailID = 63;
SELECT * FROM OrderDetails WHERE OrderDetailID = 64;
SELECT * FROM OrderDetails WHERE OrderDetailID = 65;
SELECT * FROM OrderDetails WHERE OrderDetailID = 66;
SELECT * FROM OrderDetails WHERE OrderDetailID = 67;
SELECT * FROM OrderDetails WHERE OrderDetailID = 68;
SELECT * FROM OrderDetails WHERE OrderDetailID = 69;
SELECT * FROM OrderDetails WHERE OrderDetailID = 70;
SELECT * FROM OrderDetails WHERE OrderDetailID = 71;
SELECT * FROM OrderDetails WHERE OrderDetailID = 72;
SELECT * FROM OrderDetails WHERE OrderDetailID = 73;
SELECT * FROM OrderDetails WHERE OrderDetailID = 74;
SELECT * FROM OrderDetails WHERE OrderDetailID = 75;
SELECT * FROM OrderDetails WHERE OrderDetailID = 76;
SELECT * FROM OrderDetails WHERE OrderDetailID = 77;
SELECT * FROM OrderDetails WHERE OrderDetailID = 78;
SELECT * FROM OrderDetails WHERE OrderDetailID = 79;
SELECT * FROM OrderDetails WHERE OrderDetailID = 80;
SELECT * FROM OrderDetails WHERE OrderDetailID = 81;
SELECT * FROM OrderDetails WHERE OrderDetailID = 82;
SELECT * FROM OrderDetails WHERE OrderDetailID = 83;
SELECT * FROM OrderDetails WHERE OrderDetailID = 84;
SELECT * FROM OrderDetails WHERE OrderDetailID = 85;
SELECT * FROM OrderDetails WHERE OrderDetailID = 86;
SELECT * FROM OrderDetails WHERE OrderDetailID = 87;
SELECT * FROM OrderDetails WHERE OrderDetailID = 88;
SELECT * FROM OrderDetails WHERE OrderDetailID = 89;
SELECT * FROM OrderDetails WHERE OrderDetailID = 90;
SELECT * FROM OrderDetails WHERE OrderDetailID = 91;
SELECT * FROM OrderDetails WHERE OrderDetailID = 92;
SELECT * FROM OrderDetails WHERE OrderDetailID = 93;
SELECT * FROM OrderDetails WHERE OrderDetailID = 94;
SELECT * FROM OrderDetails WHERE OrderDetailID = 95;
SELECT * FROM OrderDetails WHERE OrderDetailID = 96;
SELECT * FROM OrderDetails WHERE OrderDetailID = 97;
SELECT * FROM OrderDetails WHERE OrderDetailID = 98;
SELECT * FROM OrderDetails WHERE OrderDetailID = 99;
SELECT * FROM OrderDetails WHERE OrderDetailID = 100;
SELECT * FROM OrderDetails WHERE OrderDetailID = 101;
