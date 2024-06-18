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

select date(horolog)'66751';
select date(zut)'2023-01-01';
select date(zhorolog)'66751,,,';
select date'2023-01-01';
select date(fileman)'2970919';

select time(horolog)'56523';
select time(zut)'2023-01-01';
select time(zhorolog)',56611,,';
select time'2023-01-01';
select time(fileman)'082701';

--select time(horolog) with time zone'56523';
--select time(zut) with time zone'2023-01-01';
--select time(zhorolog) with time zone',56611,,14400'; -- time and timezone i.e. 2nd and 4th value of zhorolog
--select time with time zone'2023-01-01';
--select time(fileman) with time zone'082701';

select timestamp(horolog)'66751,56523';
select timestamp(zut)'2023-01-01';
select timestamp(zhorolog)'66751,56611,461835,14400';
select timestamp'2023-01-01';
select timestamp(fileman)'2970919.082701';

select timestamp(horolog) with time zone'66751,56523';
select timestamp(zut) with time zone'2023-01-01';
select timestamp(zhorolog) with time zone'66751,56611,461835,14400';
select timestamp with time zone'2023-01-01';
select timestamp(fileman) with time zone'2970919.082701';
