-- ##############################################################
-- #								#
-- # Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.					#
-- #								#
-- #	This source code contains the intellectual property	#
-- #	of its copyright holder(s), and is made available	#
-- #	under a license.  If you do not know the terms of	#
-- #	the license, please stop and do not read further.	#
-- #								#
-- ##############################################################

-- TNJ07 : OCTO295 : northwind schema : NATURAL JOINs remove columns with duplicate names from table

SELECT * FROM Employees NATURAL JOIN (SELECT EmployeeID, Notes, LastName FROM Employees) AS alias3;
SELECT ALL (SELECT alias1.OrderDetailID FROM OrderDetails alias1 WHERE (((15) % -(-20)) <= alias1.Quantity) ORDER BY alias1.OrderDetailID LIMIT 1) AS alias1, Employees.Photo, Employees.EmployeeID, Employees.FirstName, Employees.Notes, (SELECT alias2.ProductID FROM OrderDetails alias2 ORDER BY alias2.ProductID LIMIT 1) AS alias2, Employees.Notes FROM Employees NATURAL JOIN (SELECT DISTINCT alias3.EmployeeID, alias3.Notes, alias3.LastName FROM Employees alias3) AS alias3;

-- natural join on 2 tables with one common column
SELECT * FROM Customers NATURAL JOIN Orders;

-- natural join on 2 tables with no common column
SELECT * FROM Categories NATURAL JOIN Suppliers;

-- natural join on 2 tables with no common column but duplicate columns on right table
SELECT * FROM Shippers NATURAL JOIN (SELECT * from Categories c1, Categories c2) c3;

-- natural join on 2 tables with no common column but duplicate columns on left table
SELECT * FROM (SELECT * from Categories c1, Categories c2) c3 NATURAL JOIN Shippers;

