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

select 1.6::integer;
select 1.5::integer;
select 1.4::integer;
select -1.6::integer;
select -1.5::integer;
select -1.4::integer;
select 1.60::integer;
select 1.50::integer;
select 1.49::integer;
select -1.60::integer;
select -1.50::integer;
select -1.49::integer;
select 1.69999999::integer;
select 1.50000000::integer;
select 1.49999999::integer;
select -1.6999999::integer;
select -1.5000000::integer;
select -1.4999999::integer;
select 12.60::integer;
select 34.50::integer;
select 45.49::integer;
select -12.60::integer;
select -34.50::integer;
select -45.49::integer;
select 12348999.69999999::integer;
select 12348999.50000000::integer;
select 111111.49999999::integer;
select -12348999.6999999::integer;
select -12348999.5000000::integer;
select -111111.4999999::integer;
select 0.4::integer;
select 0.5::integer;
select 1.4000000000::integer;
select 1.5000000000::integer;
select .59999::integer;
select .49999::integer;
select 2.::integer;
select 0.::integer;

-- Following query is moved from TCT011 as it matches Postgres output with https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1208
select id from (select * from names UNION select 6.5::integer,'A','B')n1;

-- Following query is copied from TCT013 as it matches Postgres
SELECT -1.5::integer FROM names;
SELECT (-1.5)::integer FROM names;
