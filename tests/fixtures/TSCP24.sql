#################################################################
#                                                              #
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.      #
# All rights reserved.                                         #
#                                                              #
#      This source code contains the intellectual property     #
#      of its copyright holder(s), and is made available       #
#      under a license.  If you do not know the terms of       #
#      the license, please stop and do not read further.       #
#                                                              #
#################################################################

-- TSCP24 : OCTO211 : Allow NULL values in force numeric unary operation
select 'test' || +NULL;
select 1- +NULL from names;
select 1+ +NULL from names;
select 1* +NULL from names;
select 1/ +NULL from names;
select +NULL;

select 'test' || '1'::integer from names;
select 'test' || '12'::integer from names;
select 'test' || -abs(NULL) from names;
select 'test' || +abs(NULL) from names;

-- Query from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/merge_requests/1244#note_1388978516
select * from names where firstname = 'Zero' || +NULL;
