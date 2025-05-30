#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TVD0012 : YDBOctoVistA#34 Test VistA Functions
--
select DATEFORMAT(3000101.010101);
select DATEFORMAT(3000101.010101,'');
select DATEFORMAT(3000101.010101,'5Z');
select FMGET(200,.01,1);
select FMGET(200.03,.01,2,1);
select IFNULL((select NULL),'0');
select IFNULL('','0');
select IFNULL('A','0');
select LEFTY('ABCDEFGH',5);
select MPIECE('A^B','^',1);
select TOKEN('A^B','^',1);
select NUMBER('123 any road');
select PATINDEX('BQAF', 'A');
select REPLACE('ABCA', 'A', 'B');
select RIGHTY('ABCDEFGH',5);
select FMDIFF(3000102.010101,3000101.010101,2);
select FMADD(3000101.010101,1,0,0,0);
