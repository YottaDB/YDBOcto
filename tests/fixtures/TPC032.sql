#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
SELECT
    od.OrderDetailID,
    od.OrderID,
    od.ProductID,
    od.Quantity,

    p.ProductName,
    p.Price,

    o.CustomerID,
    o.EmployeeID,
    o.OrderDate,

    c.CustomerName,
    c.City,

    e.FirstName,
    e.LastName

FROM
(
    SELECT *
    FROM OrderDetails

    UNION ALL

    SELECT *
    FROM OrderDetails
) od
JOIN nwOrders o
    ON o.OrderID = od.OrderID
JOIN nwCustomers c
    ON c.CustomerID = o.CustomerID
JOIN Employees e
    ON e.EmployeeID = o.EmployeeID
JOIN Products p
    ON p.ProductID = od.ProductID

JOIN
(
    SELECT od1.ProductID, od1.OrderDetailID
    FROM OrderDetails od1
    JOIN OrderDetails od2
        ON od1.ProductID = od2.ProductID
       AND od1.OrderDetailID <= od2.OrderDetailID
) amp
    ON amp.OrderDetailID = od.OrderDetailID
   AND amp.ProductID = od.ProductID

ORDER BY
    od.ProductID,
    od.OrderDetailID,
    o.OrderID;

