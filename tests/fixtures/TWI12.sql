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

-- TWI12 : OCTO523 : Queries with lots of IN usages in WHERE or ON clause causes Octo to use too much memory and terminate abnormally

SELECT n1.id,n2.id,n3.id,n4.id,n5.id,n6.id FROM names n1, names n2, names n3, names n4, names n5, names n6 WHERE n1.id in (1,2) AND n2.id in (2,500,463,161,277,337,600,72) AND n3.id in (3,549,737) AND n4.id in (4,183,506,884) AND n5.id in (5,668,195,61) AND n6.id in (0,797,797,384,844,485,672);

SELECT n1.id,n2.id,n3.id,n4.id,n5.id,n6.id,n7.id FROM names n1, names n2, names n3, names n4, names n5, names n6, names n7 WHERE n1.id in (1,2,10,20,30,40,50) AND n2.id in (2,500,463,161,277,337,600,72) AND n3.id in (3,549,737,898,943,345) AND n4.id in (4) AND n5.id in (5,668,195,61,123,258,823) AND n6.id in (2) AND n7.id in (0,797,797,384,844,485,5);

-- Below are queries that do not take up too much memory but are there to test the number of generated physical plans
SELECT id FROM names WHERE id IN (1,2,3);
SELECT * FROM names n1 WHERE n1.firstname IN ('Zero', 'Lord');
SELECT n1.id,n2.id FROM names n1, names n2 WHERE n1.id IN (1,5) AND n2.id IN (3,4);

-- Below query tests IN where left and right side are expressions
SELECT n1.id,n2.id FROM names n1, names n2 WHERE n1.id+1 IN (2,4) AND n2.id-1 IN (NULL+1,3-1,n1.id-1,2);

