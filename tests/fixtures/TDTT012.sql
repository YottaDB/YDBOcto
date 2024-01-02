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

-- All of the following are expected to issue error
select 1.0::date;
select 1::date;
select 1::time;
select (1::time)::boolean;
select date(zut) '-27809392000000000'::date(fileman);
select date(zut) '-27809392000000000'::timestamp(fileman);
select date(zut) '-27809392000000000'::timestamp(zhorolog) with time zone;
select date(zut) '-27809392000000000'::time(zhorolog);
select date(zut) '-27809392000000000'::time(zhorolog) with time zone;
(select 'tex.+ EN-;t') union (select date'01-01-2023');
(select 'tex.3 EN-filem;t') union (select date'01-01-2023');
(select 'tex.3 EN-sele;t') union (select date'01-01-2023');
(select 'tex.testtsc3 EN-;t') union (select date'01-01-2023');
select (timestamp(horolog)'66475,3661'::date) ='01.01-2-2\277'::date;
