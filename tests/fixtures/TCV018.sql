#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Joins in view definition
-- join participants are table and view
create view TCV018_1v1 as select CustomerID,CustomerName,ContactName,Address,City,PostalCode,Country from nwCustomers;
create view TCV018_1v2 as select TCV018_1v1.customerid as ccustomerid,CustomerName,ContactName,Address,City,PostalCode,Country,OrderID,n2.CustomerID as ocustomerid,EmployeeID,OrderDate,ShipperId from TCV018_1v1,nwOrders as n2;
select * from TCV018_1v2; -- 17836 rows -- 5sec Octo
-- select * from TCV018_1v2,TCV018_1v2 as n3; -- its a 17836 * 17836 row computation. Takes too much resource so don't run it.
drop view TCV018_1v2;
drop view TCV018_1v1;

create view TCV018_1v1 as select CustomerID,CustomerName,ContactName,Address,City,PostalCode,Country from nwCustomers;
create view TCV018_1v2 as select TCV018_1v1.customerid as ccustomerid,CustomerName,ContactName,Address,City,PostalCode,Country,OrderID,n2.CustomerID as ocustomerid,EmployeeID,OrderDate,ShipperId from TCV018_1v1 inner join nwOrders as n2 on n2.customerid = TCV018_1v1.customerid;
select * from TCV018_1v2;
drop view TCV018_1v2;
drop view TCV018_1v1;

create view TCV018_1v1 as select CustomerID,CustomerName,ContactName,Address,City,PostalCode,Country from nwCustomers;
create view TCV018_1v2 as select TCV018_1v1.customerid as ccustomerid,CustomerName,ContactName,Address,City,PostalCode,Country,OrderID,n2.CustomerID as ocustomerid,EmployeeID,OrderDate,ShipperId from TCV018_1v1 left join nwOrders as n2 on n2.customerid = TCV018_1v1.customerid;
select * from TCV018_1v2;
drop view TCV018_1v2;
drop view TCV018_1v1;


create view TCV018_1v1 as select CustomerID,CustomerName,ContactName,Address,City,PostalCode,Country from nwCustomers;
create view TCV018_1v2 as select TCV018_1v1.customerid as ccustomerid,CustomerName,ContactName,Address,City,PostalCode,Country,OrderID,n2.CustomerID as ocustomerid,EmployeeID,OrderDate,ShipperId from TCV018_1v1 right join nwOrders as n2 on n2.customerid = TCV018_1v1.customerid;
select * from TCV018_1v2;
drop view TCV018_1v2;
drop view TCV018_1v1;

create view TCV018_1v1 as select CustomerID,CustomerName,ContactName,Address,City,PostalCode,Country from nwCustomers;
create view TCV018_1v2 as select TCV018_1v1.customerid as ccustomerid,CustomerName,ContactName,Address,City,PostalCode,Country,OrderID,n2.CustomerID as ocustomerid,EmployeeID,OrderDate,ShipperId from TCV018_1v1 full outer join nwOrders as n2 on n2.customerid = TCV018_1v1.customerid;
select * from TCV018_1v2;
drop view TCV018_1v2;
drop view TCV018_1v1;

create view TCV018_1v1 as select CustomerID,CustomerName,ContactName,Address,City,PostalCode,Country from nwCustomers;
create view TCV018_1v2 as select TCV018_1v1.customerid as ccustomerid,CustomerName,ContactName,Address,City,PostalCode,Country,OrderID,n2.CustomerID as ocustomerid,EmployeeID,OrderDate,ShipperId from TCV018_1v1 natural join nwOrders as n2;
select * from TCV018_1v2;
drop view TCV018_1v2;
drop view TCV018_1v1;
