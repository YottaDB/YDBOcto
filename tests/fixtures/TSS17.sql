#################################################################
#								#
# Copyright (c) 2020-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TSS17 : OCTO474 : Incorrect results when a column from a sub-query (that selects more than 1 column) is used in the ON clause of a JOIN

SELECT Employees.EmployeeID FROM Employees INNER JOIN (SELECT alias3.Notes, alias3.EmployeeID FROM Employees alias3) AS alias3 ON Employees.EmployeeID = alias3.EmployeeID;
SELECT EmployeeID,alias3.BirthDate FROM Employees INNER JOIN (SELECT alias3.BirthDate FROM Employees alias3) AS alias3 ON Employees.BirthDate = alias3.BirthDate;
SELECT EmployeeID,alias3.BirthDate FROM Employees INNER JOIN (SELECT alias3.Notes, alias3.BirthDate FROM Employees alias3) AS alias3 ON Employees.BirthDate = alias3.BirthDate;
SELECT EmployeeID,alias3.BirthDate FROM Employees INNER JOIN (SELECT MAX(alias3.Notes), alias3.BirthDate FROM Employees alias3 GROUP BY alias3.BirthDate) AS alias3 ON (((Employees.BirthDate = alias3.BirthDate)));
SELECT EmployeeID,alias3.BirthDate FROM Employees INNER JOIN (SELECT MAX(DISTINCT alias3.Notes), alias3.BirthDate FROM Employees alias3 GROUP BY alias3.BirthDate) AS alias3 ON (((Employees.BirthDate = alias3.BirthDate)));
SELECT EmployeeID,alias3.BirthDate FROM Employees INNER JOIN (SELECT MAX(DISTINCT alias3.Notes), alias3.BirthDate FROM Employees alias3 GROUP BY alias3.BirthDate HAVING alias3.BirthDate BETWEEN DATE'1968-12-08' AND DATE'1969-07-02') AS alias3 ON (((Employees.BirthDate = alias3.BirthDate)));
SELECT DISTINCT * FROM Employees INNER JOIN (SELECT MAX(DISTINCT alias3.Notes), alias3.BirthDate FROM Employees alias3 GROUP BY alias3.BirthDate HAVING alias3.BirthDate BETWEEN DATE'1968-12-08' AND DATE'1969-07-02') AS alias3 ON (((Employees.BirthDate = alias3.BirthDate)));

