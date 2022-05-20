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

-- TSCP19 : OCTO831 : IN an NOT IN operation must generate an error when the IN list has different data type members

-- IN
-- literals
select 1 in ('stf',12) from names n1;
select 1 in (12,'stf') from names n1;
select 'stf' in (12,12) from names n1;
select 'stf' in ('stf',12) from names n1;
select 'stf' in (12,'stf') from names n1;
-- column
select n1.id in ('stf',12) from names n1;
select n1.id in (n1.lastname,12) from names n1;
select n1.id in (n1.lastname,n1.id) from names n1;
select n1.id in (n1.id,n1.lastname) from names n1;
select n1.id in (n1.*,n1.id) from names n1;
select n1.id in (n1.id,n1.*) from names n1;
select n1.lastname in (n1.lastname,12) from names n1;
select n1.lastname in (n1.lastname,n1.id) from names n1;
select n1.lastname in (n1.id,n1.lastname) from names n1;
select n1.lastname in (n1.id,n1.id) from names n1;
select n1.lastname in (n1.lastname,n1.*) from names n1;
select n1.lastname in (n1.*,n1.lastname) from names n1;
select n1.* in ('stf',12) from names n1;
select n1.* in (n1.lastname,12) from names n1;
select n1.* in (n1.id,n1.lastname) from names n1;
select n1.* in (n1.lastname,n1.lastname) from names n1;
select n1.* in (n1.*,n1.lastname) from names n1;
select n1.* in (n1.lastname,n1.*) from names n1;
select n1.* in (n1.id,n1.*) from names n1;
select n1.* in (n1.*,n1.id) from names n1;
select n1.* in (n1.lastname,n1.*) from names n1;
select n1.* in (n1.*,n1.lastname) from names n1;
select n1.* in (n1.lastname,NULL) from names n1;
select n1.* in (NULL,n1.lastname) from names n1;
select n1.* in (n1.lastname) from names n1;
select n1.lastname in (n1.*) from names n1;
select n1.* in (n1.id) from names n1;
select n1.id in (n1.*) from names n1;



-- NOT IN
-- literals
select 1 not in ('stf',12) from names n1;
select 1 not in (12,'stf') from names n1;
select 'stf' not in (12,12) from names n1;
select 'stf' not in ('stf',12) from names n1;
select 'stf' not in (12,'stf') from names n1;
-- column
select n1.id not in ('stf',12) from names n1;
select n1.id not in (n1.lastname,12) from names n1;
select n1.id not in (n1.lastname,n1.id) from names n1;
select n1.id not in (n1.id,n1.lastname) from names n1;
select n1.id not in (n1.*,n1.id) from names n1;
select n1.id not in (n1.id,n1.*) from names n1;
select n1.lastname not in (n1.lastname,12) from names n1;
select n1.lastname not in (n1.lastname,n1.id) from names n1;
select n1.lastname not in (n1.id,n1.lastname) from names n1;
select n1.lastname not in (n1.id,n1.id) from names n1;
select n1.lastname not in (n1.lastname,n1.*) from names n1;
select n1.lastname not in (n1.*,n1.lastname) from names n1;
select n1.* not in ('stf',12) from names n1;
select n1.* not in (n1.lastname,12) from names n1;
select n1.* not in (n1.id,n1.lastname) from names n1;
select n1.* not in (n1.lastname,n1.lastname) from names n1;
select n1.* not in (n1.*,n1.lastname) from names n1;
select n1.* not in (n1.lastname,n1.*) from names n1;
select n1.* not in (n1.id,n1.*) from names n1;
select n1.* not in (n1.*,n1.id) from names n1;
select n1.* not in (n1.lastname,n1.*) from names n1;
select n1.* not in (n1.*,n1.lastname) from names n1;
select n1.* not in (n1.lastname,NULL) from names n1;
select n1.* not in (NULL,n1.lastname) from names n1;
select n1.* not in (n1.lastname) from names n1;
select n1.lastname not in (n1.*) from names n1;
select n1.* not in (n1.id) from names n1;
select n1.id not in (n1.*) from names n1;
