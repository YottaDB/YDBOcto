#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TVD0011 : YDBOctoVistA#34 VistA Functions need to handle SQL NULL
select CURRTIMESTAMP((select null)) IS NULL as CURRTIMESTAMP;
select GETDATE((select null)) IS NULL as GETDATE;
select DATEFORMAT((select null)) IS NULL as DATEFORMAT1;
select DATEFORMAT((select null),'5Z') IS NULL as DATEFORMAT2;
select FMGET(200,.01,(select null)) IS NULL as FMGET1;
select FMGET(200,.01,(select null),(select null)) IS NULL as FMGET2;
select FMGET(200,.01,1,(select null),(select null)) IS NULL as FMGET3;
select IFNULL((select null),'foo') IS NULL as IFNULL;
select LEFTY((select null),5) IS NULL as LEFTY;
select MPIECE((select null),'^',1) IS NULL as MPIECE;
select NUMBER((select null)) IS NULL as NUMBER;
select PATINDEX((select null), 'A') IS NULL as PATINDEX;
select REPLACE((select null), 'A', 'B') IS NULL as REPLACE;
select RIGHTY((select null),5) IS NULL as RIGHTY;
select TOKEN((select null),'^',1) IS NULL as TOKEN;
select FMDIFF((select null),(select null),(select null)) IS NULL as FMDIFF;
select FMADD((select null),1,2,3,4) IS NULL as FMADD;
