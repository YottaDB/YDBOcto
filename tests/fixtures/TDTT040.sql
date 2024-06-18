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

-- horolog
select date(horolog)'-366';
select date_to_zhorolog(date(horolog) '-366');
select date(horolog)'-365';
select date(horolog)'2980014';
select date(horolog)'2980013';
select date(horolog)'';

select time(horolog)'-1';
select time(horolog)'0';
select time(horolog)'86400';
select time(horolog)'86399';
-- select time(horolog) with time zone'0';
-- select time(horolog) with time zone'86399';
select time(horolog)'';
-- select time(horolog) with time zone'';

select timestamp(horolog)'-365,-1';
select timestamp(horolog)'-366,0';
select timestamp(horolog)'2980014,0';
select timestamp(horolog)'2980013,86400';
select timestamp(horolog)'-365,0';
select timestamp(horolog)'2980013,86399';
select timestamp(horolog) with time zone'-365,0';
select timestamp(horolog) with time zone'2980013,86399';
select timestamp(horolog)'';
select timestamp(horolog)'0,';
select timestamp(horolog)',0';
select timestamp(horolog) with time zone'';

--zhorolog
select date(zhorolog)'-366,,,';
select date(zhorolog)'-365,,,';
select date(zhorolog)'2980013,,,';
select date(zhorolog)'2980014,,,';
select date(zhorolog)',,,';
select time(zhorolog)',-1,,';
select time(zhorolog)',0,,';
select time(zhorolog)',0,-1,';
select time(zhorolog)',0,0,';
select time(zhorolog)',86399,,';
select time(zhorolog)',86400,,';
select time(zhorolog)',86399,999999,';
select time(zhorolog)',86399,100000,';
select time(zhorolog)',,,';
-- select time(zhorolog) with time zone',0,,43201';
-- select time(zhorolog) with time zone',-1,,43200';
-- select time(zhorolog) with time zone',0,,43200';
-- select time(zhorolog) with time zone',0,-1,43200';
-- select time(zhorolog) with time zone',0,0,43200';
-- select time(zhorolog) with time zone',86399,,-50400';
-- select time(zhorolog) with time zone',86400,,-50400';
-- select time(zhorolog) with time zone',86399,,-50401';
-- select time(zhorolog) with time zone',86399,999999,-50400';
-- select time(zhorolog) with time zone',86399,1000000,-50400';
-- select time(zhorolog) with time zone',,,';
select timestamp(zhorolog)'-366,0,,';
select timestamp(zhorolog)'-365,-1,,';
select timestamp(zhorolog)'-365,0,,';
select timestamp(zhorolog)'-365,0,-1,';
select timestamp(zhorolog)'-365,0,0,';
select timestamp(zhorolog)'2980013,86399,,';
select timestamp(zhorolog)'2980013,86400,,';
select timestamp(zhorolog)'2980013,86399,999999,';
select timestamp(zhorolog)'2980013,86399,1000000,';
select timestamp(zhorolog)',,,';
select timestamp(zhorolog) with time zone'-365,0,,43201';
select timestamp(zhorolog) with time zone'-366,0,,43200';
select timestamp(zhorolog) with time zone'-365,-1,,43200';
select timestamp(zhorolog) with time zone'-365,0,,43200';
select timestamp(zhorolog) with time zone'-365,0,-1,43200';
select timestamp(zhorolog) with time zone'-365,0,0,43200';
select timestamp(zhorolog) with time zone'2980013,86399,,-50400';
select timestamp(zhorolog) with time zone'2980014,86399,,-50400';
select timestamp(zhorolog) with time zone'2980013,86400,,-50400';
select timestamp(zhorolog) with time zone'2980013,86399,,-50401';
select timestamp(zhorolog) with time zone'2980013,86399,999999,-50400';
select timestamp(zhorolog) with time zone'2980013,86399,999999,-50401';
select timestamp(zhorolog) with time zone'2980013,86399,1000000,-50400';
select timestamp(zhorolog) with time zone'2980013,86400,999999,-50400';
select timestamp(zhorolog) with time zone'2980014,86399,999999,-50400';
select timestamp(zhorolog) with time zone',,,';

-- fileman
select date(fileman)'0000000'; -- in-exact date(fileman)
select date(fileman)'0000101';
select date(fileman)'9991231';
select date(fileman)'10001231'
select date(fileman)'-9991231';
select date(fileman)'+9991231';
select date(fileman)'';
select time(fileman)'0';
select time(fileman)'';
-- select time(fileman) with time zone'0';
-- select time(fileman) with time zone'';
select timestamp(fileman)'0000000.000000'; -- in-exact date(fileman)
select timestamp(fileman)'0000101.000000';
select timestamp(fileman)'9991231.240000';
select timestamp_to_fileman(timestamp(fileman) '9991231.24');
select timestamp(fileman) '9991231.24';
select timestamp(fileman)'10001231.000000'
select timestamp(fileman)'-9991231.000000';
select timestamp(fileman)'+9991231.000000';
select timestamp(fileman)'0000000';
select timestamp(fileman)'';
select timestamp(fileman) with time zone'0.0';
select timestamp(fileman) with time zone'0.';
select timestamp(fileman) with time zone'';

--zut
select date(zut)'-62167201439000000';
select date(zut)'-62167201438000000';
select date(zut)'-62167219300000000';
select date(zut)'-62167219200000000';
select date(zut)'253402232400000000';
select date(zut)'253402232500000000';
select date(zut)'253402214400000000';
select date(zut)'253402214500000000';
select date(zut)'';
select time(zut)'-2209057300000000';
select time(zut)'-2209057200000000';
select time(zut)'-2208970800000001';
select time(zut)'-2208970800000002';
select time(zut)'';
select timestamp(zut)'-62167201439000000';
select timestamp(zut)'-62167201438000000';
select timestamp(zut)'-62167219200000000';
select timestamp(zut)'-62167219300000000';
select timestamp(zut)'253402318799999999';
select timestamp(zut)'253402318800000000';
select timestamp(zut)'253402300799999999';
select timestamp(zut)'253402300800000000';
select timestamp(zut)'';
-- select time(zut) with time zone'0';
select time with time zone'';
select timestamp with time zone'0';
select timestamp with time zone'';

-- text
select date '0000-01-01';
select date '0000-00-01';
select date '0000-01-00';
select date '1000-01-010';
select date '9999-12-31';
select date'0100-01-01';
select date'0010-01-01';
select date'0001-01-01';
select date'';

select time'00:00:00.000000';
select time'00:00:00';
select time'23:59:59.999999';
select time'23:59:59';
select time'23:59:59.1000000';
select time'24:00:00.000000';
select time'-01:00:00';
select time'01:-01:00';
select time'00:60:00';
select time'00:00:60';
select time'';

-- select time with time zone'00:00:00.000000-16:00';
-- select time with time zone'00:00:00.000000-15:59';
-- select time with time zone'00:00:00.000000+15:59';
-- select time with time zone'00:00:00.000000+16:00';
-- select time with time zone'23:59:59.999999-16:00';
-- select time with time zone'23:59:59.999999-15:59';
-- select time with time zone'23:59:59.999999+15:59';
-- select time with time zone'23:59:59.999999+16:00';
-- select time with time zone'';

select timestamp'0000-01-01';
select timestamp'0100-01-01';
select timestamp'0010-01-01';
select timestamp'0001-01-01';
select timestamp'0000-01-01 00:00:00';
select timestamp'0100-01-01 00:00:00';
select timestamp'0010-01-01 00:00:00';
select timestamp'0001-01-01 00:00:00';
select timestamp'1000-01-010 00:00:00';
select timestamp'0000-01-01 00:00:00.000000';
select timestamp '9999-12-31 23:59:59.999999';
select timestamp '9999-12-31 23:59:59';
select timestamp '9999-12-31 23:59:60';
select timestamp '0000-01-01 23:60:59';
select timestamp '9999-12-31 23:59:59.1000000';
select timestamp '';

select timestamp with time zone'0000-01-01 00:00:00.000000-16:00';
select timestamp with time zone'0000-01-01 00:00:00.000000-15:59';
select timestamp with time zone'0000-01-01 00:00:00.000000+15:59';
select timestamp with time zone'0000-01-01 00:00:00.000000+16:00';
select timestamp with time zone'9999-12-31 23:59:59.999999-16:00';
select timestamp with time zone'9999-12-31 23:59:59.999999-15:59';
select timestamp with time zone'9999-12-31 23:59:59.999999+15:59';
select timestamp with time zone'9999-12-31 23:59:59.999999+16:00';
select timestamp with time zone'';


-- https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1404#note_1670617393
select date(horolog)'66475'; -- 2023-01-01
select date(horolog)'-66475'; -- ERR_INVALID_DATE_TIME_FORMAT
select date(zut)'66475';
select date(zut)'-66475';
select date(zut)'0';
select timestamp(zut)'66475';
select timestamp(zut)'-66475';
select timestamp(zut)'999999';
select timestamp(zut)'-999999';
select timestamp(zut)'-1000000';
select timestamp(zut)'-1';
select timestamp(zut)'0';
select timestamp(zut)'1';
select timestamp(zut)'-1999999';

-- Some more edge cases
-- Non of the following should generate an error
select timestamp with time zone'2024-01-01 01:01:01.7323';
select timestamp with time zone'2024-01-01 01:01:01.732300-05:00';
select timestamp with time zone'2024-01-01 01:01:01.73230-05:00';
select timestamp with time zone'2024-01-01 01:01:01.7323-05:00';
