#################################################################
#								#
# Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select * from names where firstname BETWEEN 'Acid' and 'Lord';
select * from names where firstname BETWEEN 'Acid' and 'Lorc';
select * from names where firstname BETWEEN 'Acic' and 'Lorc';
select * from names where firstname BETWEEN 'Acie' and 'Lorc';
select * from names where firstname BETWEEN 'Acie' and 'Lord';
select * from names where firstname NOT BETWEEN 'Acid' and 'Lord';
select * from names where firstname NOT BETWEEN 'Acid' and 'Lorc';
select * from names where firstname NOT BETWEEN 'Acic' and 'Lorc';
select * from names where firstname NOT BETWEEN 'Acie' and 'Lorc';
select * from names where firstname NOT BETWEEN 'Acie' and 'Lord';
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id AND n1.id BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id AND n1.id NOT BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id OR n1.id BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id = n2.id OR n1.id NOT BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id NOT BETWEEN 2 and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id BETWEEN 2 and n2.id;
select n1.id,n2.id from names n1 inner join names n2 on n1.id NOT BETWEEN 2 and n2.id;
select n1.id,n2.id from names n1 inner join names n2 on n1.id BETWEEN n2.id and 4;
select n1.id,n2.id from names n1 inner join names n2 on n1.id NOT BETWEEN n2.id and 4;
select * from names n1 where n1.id BETWEEN 2 and 4;
select * from names n1 where n1.id NOT BETWEEN 2 and 4;
select * from names n1 where n1.id > 2  OR n1.id BETWEEN 1 and 4;
select * from names n1 where n1.id > 2  OR n1.id NOT BETWEEN 1 and 4;
select * from names n1 where n1.id > 2  AND n1.id BETWEEN 1 and 4;
select * from names n1 where n1.id > 2  AND n1.id NOT BETWEEN 1 and 4;
select * from (select * from (select * from names n1 where EXISTS (select n2.id from names n2 where n2.id BETWEEN n1.id and n1.id + 2)) n3) n4;
select * from (select * from (select * from names n1 where EXISTS (select n2.id from names n2 where n2.id NOT BETWEEN n1.id and n1.id + 2)) n3) n4;
