#################################################################
#								#
# Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE TABLE nwCustomers (CustomerID INTEGER PRIMARY KEY, CustomerName VARCHAR(48), ContactName VARCHAR(32), Address VARCHAR(64), City VARCHAR(32), PostalCode VARCHAR(16), Country VARCHAR(32)) GLOBAL "^nwCustomers";
CREATE TABLE nwOrders (OrderID INTEGER PRIMARY KEY, CustomerID INTEGER, EmployeeID INTEGER, OrderDate DATE, ShipperID INTEGER) GLOBAL "^nwOrders" readonly;
CREATE TABLE Shippers (ShipperID INTEGER PRIMARY KEY, ShipperName VARCHAR(32), Phone VARCHAR(14)) GLOBAL "^Shippers";
CREATE TABLE Suppliers (SupplierID INTEGER PRIMARY KEY, SupplierName VARCHAR(48), ContactName VARCHAR(32), Address VARCHAR(64), City VARCHAR(32), PostalCode VARCHAR(16), Country VARCHAR(32), Phone VARCHAR(15)) GLOBAL "^Suppliers";
CREATE TABLE Products (ProductID INTEGER PRIMARY KEY, ProductName VARCHAR(48), SupplierID INTEGER, CategoryID INTEGER, Unit VARCHAR(32), Price NUMERIC) GLOBAL "^Products";
CREATE TABLE OrderDetails (OrderDetailID INTEGER PRIMARY KEY, OrderID INTEGER, ProductID INTEGER, Quantity INTEGER) GLOBAL "^OrderDetails";
CREATE TABLE Categories (CategoryID INTEGER PRIMARY KEY, CategoryName VARCHAR(32), Description VARCHAR(64)) GLOBAL "^Categories";
CREATE TABLE Employees (EmployeeID INTEGER PRIMARY KEY, LastName VARCHAR(32), FirstName VARCHAR(32), BirthDate DATE, Photo VARCHAR(16), Notes VARCHAR(512)) GLOBAL "^Employees" readonly;
