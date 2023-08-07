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
select CURRTIMESTAMP((select null::varchar)) IS NULL as CURRTIMESTAMP;
select GETDATE((select null::varchar)) IS NULL as GETDATE;
select DATEFORMAT((select null::numeric),'5Z') IS NULL as DATEFORMAT;
select FMGET(200::numeric,.01,(select null::numeric)) IS NULL as FMGET1;
select FMGET(200::numeric,.01,(select null::numeric),(select null::numeric)) IS NULL as FMGET2;
select FMGET(200::numeric,.01,1::numeric,(select null::numeric),(select null::numeric)) IS NULL as FMGET3;
select IFNULL((select null::varchar),'foo') IS NULL as IFNULL;
select LEFTY((select null::varchar),5) IS NULL as LEFTY;
select MPIECE((select null::varchar),'^',1) IS NULL as MPIECE;
select NUMBER((select null::varchar)) IS NULL as NUMBER;
select PATINDEX((select null::varchar), 'A') IS NULL as PATINDEX;
select REPLACE((select null::varchar), 'A', 'B') IS NULL as REPLACE;
select RIGHTY((select null::varchar),5) IS NULL as RIGHTY;
select TOKEN((select null::varchar),'^',1) IS NULL as TOKEN;
select FMDIFF((select null::numeric),(select null::numeric),(select null::integer)) IS NULL as FMDIFF;
select FMADD((select null::numeric),1,2,3,4) IS NULL as FMADD;
