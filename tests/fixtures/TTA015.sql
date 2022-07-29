#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- Query which found the #880 issue
SELECT t1.lastName as y,t2.* as lastname FROM names t1 LEFT JOIN (SELECT lastName FROM names group by lastname) AS t2 ON (t1.firstName <= 'Acid') WHERE exists(select (t1.id <=1) AND (t2.* IS NULL)) order by t2.*;

-- Simplified version of the query and queries where the error causing subquery is placed in different clauses
SELECT 1 FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') WHERE exists(select t2.* is NULL);
SELECT exists (select t2.* is NULL) FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid');
SELECT 1 FROM names t1 LEFT JOIN (SELECT lastName FROM names) AS t2 ON (t1.firstName <= 'Acid') order by exists(select t2.* is NULL);
