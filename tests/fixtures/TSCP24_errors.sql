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
select 1% +NULL from names; -- doesn't error
select -NULL;
select 'test' || -(select null) from names;
select 'test' || +(select null) from names; -- doesn't error as type inferred here is null where Postgres infers the type as STRING
select 'test' || -NULL::varchar from names;

-- Following tests although not related to NULL values were created while working on the NULL related issue and is kept as it
-- acts as an additional test of the unary + and -
select 'test' || +'test';
select 'test' || -'test';
select +'test';
select -'test';
select -TRUE;
select +TRUE;
select 'test' || +TRUE;
select 'test' || -TRUE;
select +lastname from names;
select -lastname from names;
select 'test' || +'test'::integer from names;
select 'test' || +abs('test') from names;

-- Following query doesn't issue any error in Postgres but Octo generated an error of type ERR_TYPE_NOT_COMPATIBLE
-- YDBOcto#973 tracks this issue and the following test is retained here to capture this behavior in the test system
select null || +(select 1) from names;
select null || -(select 1) from names;

-- Following query doesn't issue any error in Postgres but Octo generates an error of type ERR_UKNOWN_FUNCTION
-- YDBOcto#974 tracks this issue and the following test is retained here to capture this behavior in the test system
-- In Octo, below usage of abs results in the following error
--   ERR_UNKNOWN_FUNCTION: No function ABS defined with given parameter types (VARCHAR)
select 'test' || +abs('12') from names;
select 'test' || -abs('12') from names;
