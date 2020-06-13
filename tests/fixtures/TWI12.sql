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

-- TWI12 : OCTO523 : Reduce Octo memory usage during logical plan phase by an order of magnitude for lots of IN usage

SELECT n1.id,n2.id,n3.id,n4.id,n5.id,n6.id
  FROM names n1, names n2, names n3, names n4, names n5, names n6
 WHERE n1.id in (1,2)
   AND n2.id in (2,500,463,161,277,337,600,72)
   AND n3.id in (3,549,737)
   AND n4.id in (4,183,506,884)
   AND n5.id in (5,668,195,61)
   AND n6.id in (0,797,797,384,844,485,672);

