#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

SELECT * FROM myTable;
SELECT "Hello world!" FROM helloTable;
SELECT toppings.name
FROM people
INNER JOIN people_toppings
ON (people.id = people_toppings.people_id)
INNER JOIN toppings
ON (toppings.id = people_toppings.toppings_id)
WHERE people.name IN ("Christopher", "Charles", "Bhaskar",
  "Narayanan", "Steve", "Ran")
GROUP BY toppings.name
HAVING COUNT(people.id) > 5;
SELECT * FROM tableA
INNER JOIN (SELECT personId as id
  FROM tableB WHERE AGE < 15) as tableC
ON (tableA.id = tableC.id);
-- Many of these examples were from http://www.complexsql.com/complex-sql-queries-examples-with-answers/
Select distinct Salary from Employee e1 where 2=(Select count(distinct Salary) from Employee e2 where e1.salary<=e2.salary);
Select * from Employee a where rowid <>( select max(rowid) from Employee b where a.Employee_num=b.Employee_num);
Select Employee_name,Salary/12 as `Monthly Salary` from employee;
Select * from Employee where Rownum =1;
Select * from Employee where Rowid=(select max(Rowid) from Employee);
Select * from Employee where Rownum <= 5;

select distinct salary from employee a where 3 >= (select count(distinct salary) from employee b where a.salary <= b.salary) order by a.salary desc;
select min(salary)from(select distinct salary from emp order by salary desc)where rownum<=3;
-- Some examples using UNION should be added after it is supported
SELECT 5+5/5 FROM A;
