#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select n1.id,n2.id from names n1 full join names n2 on n1.id = n2.id or exists (select * from names);

select n1.id,n2.id from names n1 full join names n2 on n1.id = n2.id or not exists (select * from names);

select n1.id,n2.id from names n1 full join names n2 on n1.id = n2.id or exists (select * from names where n1.id < n2.id);

select n1.id,n2.id from names n1 full join names n2 on n1.id = n2.id or not exists (select * from names where n1.id < n2.id);

select n1.id,n2.id from names n1 full join names n2 on (n1.id = (select n3.id from names n3 where n1.id = n3.id));

select n1.id,n2.id from names n1 full join names n2 on (1 = (select n3.id from names n3 where n1.id = n3.id));

select n1.id,n2.id from names n1 full join names n2 on exists (select * from names);

select n1.id,n2.id from names n1 full join names n2 on not exists (select * from names);

select n1.id,n2.id from names n1 full join names n2 on exists (select * from names where n1.id < n2.id);

select n1.id,n2.id from names n1 full join names n2 on not exists (select * from names where n1.id < n2.id);
