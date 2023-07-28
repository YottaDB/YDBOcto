#################################################################
#								#
# Copyright (c) 2019-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TGB01 : OCTO55 : GROUP BY and AGGREGATE FUNCTIONS

--> Below query should error out because aggregate functions are not allowed in GROUP BY
SELECT COUNT(CustomerID),Country FROM nwCustomers GROUP BY Country,COUNT(CustomerID);

--> Below should error out because AVG and SUM cannot operate on VARCHAR type (i.e. string literals), only on INTEGER/NUMERIC
SELECT SUM(firstname) FROM names;
SELECT AVG(firstname) FROM names;
SELECT SUM(firstname || 'abcd') FROM names;
SELECT AVG(firstname || 'abcd') FROM names;
SELECT SUM('abcd' || firstname) FROM names;
SELECT AVG('abcd' || firstname) FROM names;
SELECT SUM(Country) FROM nwCustomers;
SELECT AVG(Country) FROM nwCustomers;

--> Below should issue error since COUNT(DISTINCT *) is not a valid usage
SELECT COUNT(DISTINCT *) FROM nwCustomers c LEFT JOIN nwOrders o ON c.CustomerID = o.CustomerID;

--> Below should error out because "abcd" is an invalid column name
SELECT COUNT(abcd) FROM names;

--> Below should error out because aggregate function calls cannot be nested
SELECT 1+2*AVG(AVG(id)) FROM names;
SELECT 1+COUNT(1+COUNT(id)) FROM names;

--> Below should error out because aggregate functions are not allowed in WHERE
SELECT COUNT(OrderID),CustomerID FROM nwOrders WHERE COUNT(OrderID)>10 GROUP BY CustomerID ORDER BY COUNT(OrderID) DESC, CustomerID;
SELECT * FROM names where firstname = count(*);

--> Below should error out because aggregate functions are not allowed in WHERE clause even in a sub-query
SELECT * FROM names n1 WHERE n1.firstName IN (SELECT n2.firstname FROM names n2 where n2.firstname = MAX(n2.lastname));

--> Below should error out because GROUP BY is needed for Country due to prior use of aggregate function COUNT(CustomerID)
SELECT COUNT(CustomerID),Country FROM nwCustomers;

--> Below should error out because GROUP BY is needed for Country due to later use of aggregate function COUNT(CustomerID)
SELECT Country,COUNT(CustomerID) FROM nwCustomers;

--> Below should error out because OrderID used in ORDER BY should be used with an aggregate function.
SELECT COUNT(OrderID),CustomerID FROM nwOrders GROUP BY CustomerID ORDER BY OrderID DESC, CustomerID;

--> Below should error out because GROUP BY is needed for EmployeeID
SELECT COUNT(OrderID),CustomerID,EmployeeID FROM nwOrders GROUP BY CustomerID ORDER BY COUNT(OrderID) desc, CustomerID;

--> Below should error out because o.OrderID should appear in GROUP BY or used in an aggregate function
SELECT c.CustomerID,o.OrderID FROM nwCustomers c LEFT JOIN nwOrders o ON c.CustomerID = o.CustomerID GROUP BY c.CustomerID;

--> Below should error out because of two errors.
-->	a) aggregate function (COUNT(c.CustomerID) above) cannot be inside JOIN conditions
-->	b) o.OrderID should appear in GROUP BY or used in an aggregate function
SELECT c.CustomerID,o.OrderID FROM nwCustomers c LEFT JOIN nwOrders o ON COUNT(c.CustomerID) = o.CustomerID GROUP BY c.CustomerID;

--> Below should error out because the same column cannot be used inside and outside an aggregate function without a GROUP by.
SELECT COUNT(CustomerID),CustomerID FROM nwCustomers;

--> Below should error out because sub-query uses ungrouped column from outer query
SELECT COUNT(id),(SELECT n2.id FROM names n2 WHERE n2.id = n1.id) FROM names n1;
SELECT (SELECT id FROM names n1 WHERE n2.id = 2),COUNT(n2.id) FROM names n2;
SELECT (SELECT n3.id FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id ORDER BY n1.id),COUNT(n1.id) FROM names n1;
SELECT (SELECT n3.id FROM names n3 LEFT JOIN names n4 ON n4.id = n1.id ORDER BY n1.id),COUNT(n1.id),(SELECT n2.id FROM names n2 WHERE n2.id = n1.id) FROM names n1;
SELECT COUNT(id),(SELECT * from (select n2.id FROM names n2 WHERE n2.id = n1.id) n3) FROM names n1;

--> Below should error out because GROUP BY is done on firstname but * usage in SELECT implies columns other than country
--> (e.g. lastname) are selected without any GROUP BY or AGGREGATE function use. And should be disallowed.
SELECT * FROM names GROUP BY firstname;
SELECT * FROM (SELECT * FROM names) GROUP BY firstname;

--> Below should error out even though GROUP BY column is the primary key. The first of the 2 queries below works fine in
--> Postgres whereas the second one does not. I suspect this is because in the 1st query, we know for sure that the GROUP BY
--> column is a primary key column and hence the GROUP by can be ignored. Whereas in the 2nd query, the output comes from
--> a sub-query where we are not guaranteed the primary key assumption. In any case, Octo issues error for both cases at least now.
SELECT * FROM names GROUP BY id;
SELECT * FROM (SELECT * FROM names ) n1 GROUP BY id;

--> Below should error out with "More than one row returned by a subquery used as an expression"
SELECT id,firstname FROM names n1 WHERE id < (SELECT MAX(n2.id) FROM names n2 GROUP BY n2.id,n1.id);
SELECT id,firstname FROM names n1 WHERE id < (SELECT MAX(n2.id) FROM names n2 GROUP BY n1.id,n2.id);
SELECT id,firstname FROM names n1 WHERE id < (SELECT MAX(n2.id) FROM names n2 GROUP BY n1.id,n2.id,n1.id);

--> Below should error out because GROUP BY is needed for Country in SELECT column list
--> due to use of aggregate function COUNT(CustomerID) in HAVING clause
SELECT Country FROM nwCustomers WHERE Country = 'Abcd' HAVING COUNT(CustomerID) = 1;

--> Below should error out because HAVING clause cannot use column aliases defined in the SELECT column list
SELECT COUNT(OrderID),MAX(OrderID) as maxo,MIN(OrderID) as mino,MAX(CustomerID) as maxc,MIN(CustomerID) as minc,EmployeeID FROM nwOrders WHERE EmployeeID BETWEEN 0 AND 7 GROUP BY EmployeeID HAVING maxo > 10400 AND mino < 10275;

--> Below should error out because GROUP BY is needed for EmployeeID if used in the HAVING clause without aggregate function
SELECT COUNT(OrderID),CustomerID FROM nwOrders GROUP BY CustomerID HAVING COUNT(OrderID) BETWEEN 3 AND 6 AND EmployeeID BETWEEN 2 and 5 ORDER BY COUNT(OrderID) desc, CustomerID;

--> Below is an example of using HAVING without GROUP BY.
--> This should error out because EmployeeID should be used inside aggregate function or in a GROUP BY
SELECT 1 FROM nwOrders HAVING MAX(OrderID) BETWEEN 10250 AND 10500 ORDER BY EmployeeID desc;

--> Below should error out n2.id and n2.id need to be used with aggregate functions inside the sub-queries
SELECT (SELECT n3.id FROM names n3 ORDER BY COUNT(n1.id)),(SELECT n2.id FROM names n2 WHERE n2.id = n1.id) FROM names n1;
SELECT (SELECT n3.id FROM names n3 WHERE n3.id = n1.id),(SELECT n2.id FROM names n2 ORDER BY COUNT(n1.id)) FROM names n1;
SELECT (SELECT n3.id FROM names n3 WHERE n3.id = n1.id),(SELECT n2.id FROM names n2 ORDER BY COUNT(n1.id)) FROM names n1 GROUP BY n1.id;

--> OCTO786 : Below should error because non-grouped columns cannot be outside of aggregate functions in HAVING clause
select 1 from names n1 having n1.firstname = 'Acid';
select 1 from names n1 having n1.id is null;
select 1 from names n1 having n1.* is null;

-- OCTO786 : Below should error because non-grouped column n1.firstname is used inside HAVING clause in a sub-query
-- Below tests https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/786#note_893116624
SELECT COUNT(firstname) FROM names n1 HAVING EXISTS (select n1.firstname);
-- Below is a slightly fancier version of the previous query using deeper sub-queries.
SELECT COUNT(firstname) FROM names n1 HAVING EXISTS (SELECT COUNT(firstname) FROM names n2 WHERE 'Zero' IN (select n1.firstname));

