#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TWO18 : OCTO782 : Fix incorrect results when WHERE clause has IS NOT NULL and FROM/JOIN list has more than one table

-- Below is a query from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/782#description
select count(n1.id),count(n2.*) from names n1 inner join names n2 on n1.id = n2.id where (n2.lastName IS NOT NULL) AND (n1.id IS NOT NULL) order by 1,2;

-- Below are queries from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/782#note_730500784
select count(n1.*) from names n1 where (n1.id IS NOT NULL) AND (n1.lastName IS NULL) AND (n1.lastName IS NOT NULL) AND (n1.firstName >= 'Joey') order by 1;
select count(n1.*) from names n1 where (n1.lastName IS NULL) AND (n1.lastName IS NOT NULL) order by 1;
select count(n1.*),count(n2.*) from names n1 inner join names n2 on n2.firstName = n1.firstName where (n2.id < 0) OR (n1.lastName <= 'Burn') OR (n2.lastName IS NOT NULL) group by n1.* order by 1,2;
select count(n1.id),count(n2.id),count(n3.*) from names n1 inner join names n2 on n2.firstName IS NOT NULL inner join names n3 on n3.id = n1.id where (n1.firstName >= 'Zero') OR (n3.lastName IS NOT NULL) group by n3.* order by 1,2,3;
select count(n1.*),count(n2.*) from names n1 inner join names n2 on n1.firstName = n2.firstName where (n2.lastName = 'Nikon') OR (n2.lastName <= 'Killer') AND (n1.firstName <= 'Lord') AND (n1.lastName >= 'Killer') AND (n1.lastName <= NULL) OR (n2.lastName IS NOT NULL) group by n1.* order by 1,2;

