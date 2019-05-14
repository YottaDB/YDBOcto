CREATE TABLE Customers (CustomerID INTEGER PRIMARY KEY, CustomerName VARCHAR(32), ContactName VARCHAR(32), Address VARCHAR(64), City VARCHAR(32), PostalCode VARCHAR(16), Country VARCHAR(32)) GLOBAL "^Customers(keys(""CustomerID""))";
CREATE TABLE Orders (OrderID INTEGER PRIMARY KEY, CustomerID INTEGER, EmployeeID INTEGER, OrderDate VARCHAR(16), ShipperID INTEGER) GLOBAL "^Orders(keys(""OrderID""))";
CREATE TABLE Shippers (ShipperID INTEGER PRIMARY KEY, ShipperName VARCHAR(32), Phone VARCHAR(12)) GLOBAL "^Shippers(keys(""ShipperID""))";
CREATE TABLE Suppliers (SupplierID INTEGER PRIMARY KEY, SupplierName VARCHAR(32), ContactName VARCHAR(32), Address VARCHAR(64), City VARCHAR(32), PostalCode VARCHAR(16), Country VARCHAR(32), Phone VARCHAR(12)) GLOBAL "^Suppliers(keys(""SupplierID""))";
CREATE TABLE Products (ProductID INTEGER PRIMARY KEY, ProductName VARCHAR(32), SupplierID INTEGER, CategoryID INTEGER, Unit VARCHAR(32), Price VARCHAR(8)) GLOBAL "^Products(keys(""ProductID""))";
CREATE TABLE OrderDetails (OrderDetailID INTEGER PRIMARY KEY, OrderID INTEGER, ProductID INTEGER, Quantity INTEGER) GLOBAL "^OrderDetails(keys(""OrderDetailID""))";
CREATE TABLE Categories (CategoryID INTEGER PRIMARY KEY, CategoryName VARCHAR(32), Description VARCHAR(64)) GLOBAL "^Categories(keys(""CategoryID""))";
CREATE TABLE Employees (EmployeeID INTEGER PRIMARY KEY, LastName VARCHAR(32), FirstName VARCHAR(32), BirthDate VARCHAR(10), Photo VARCHAR(16), Notes VARCHAR(128)) GLOBAL "^Employees(keys(""EmployeeID""))";
