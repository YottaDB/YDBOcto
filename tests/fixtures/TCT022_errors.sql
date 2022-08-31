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

select (select 'a'||'b'):: integer;
select 'ab' :: integer;
select 'ab'::integer;
select '1.0'::integer;
select 't'::integer;
select 'f'::integer;
select 'test'::bool;
select 2.0::bool;
select 'a'::bool;
select -1 ::bool;
select '.1'::integer;
select '1.'::integer;
select '+1.0'::integer;
select '-1.0'::integer;
select '+1-.0'::numeric;
select ' -1. 0 '::numeric;
select '1 2'::integer;
select '              '::numeric;
select '              '::integer;
select '              '::bool;
select '.'::integer;
select '.'::numeric;
select '.  '::numeric;
select '.   '::integer;
select '.'::integer;
select '+'::integer;
select '+'::numeric;
select '+   '::integer;
select '+   '::numeric;
select true or 'a'::boolean; -- Copied from #559
select TRUE::numeric(2);
select 'abc'::numeric(2);
select 'abc'::numeric(2,1);
select '++1'::integer;
select '1--'::integer;
select '++1'::numeric;
select '1--'::numeric;
select '1.1.0'::numeric;
select '1.1.0'::numeric(1,1);
select '1.1.0'::numeric(1);
select '1.1.1'::integer;
select '+11234+1231455'::integer;
select '12112345-12'::integer;
select '-12112345-12'::integer;
select '+1231+123'::numeric;
select '1245-1123'::numeric;
select '+1231+123'::numeric;
select '-1245-1123'::numeric;

--Below queries will have NULL_VALUE type as the source type so we are not expecting it to go through the string to numerc/integer cast validation logic. Hence no errors. May change if Octo distinguishes between empty string and NULL value.
select ''::numeric;
select ''::integer;
select ''::bool;

-- Although the following is a valid query it is added to this fixture as the result is in Octo truncates the decimal portion of
--   the numeric whereas Postgres retains the numeric value as it is in the string.
select '1.0'::NUMERIC;

-- Following tests basic invalid conversions between literal types
select FALSE::numeric; -- error
select 1.1::boolean; -- error
