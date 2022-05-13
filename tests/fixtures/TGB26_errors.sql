#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

CREATE FUNCTION SAMEVALUE(INTEGER) RETURNS INTEGER AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(NUMERIC) RETURNS NUMERIC AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(BOOLEAN) RETURNS BOOLEAN AS $$samevalue^functions;
CREATE FUNCTION SAMEVALUE(VARCHAR) RETURNS VARCHAR AS $$samevalue^functions;
CREATE FUNCTION PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;


-- Functions in GroupBy
-- Using id
select samevalue(id) from names group by samevalue(id) having 1=id; -- Postgres error id in having not in GroupBy
select samevalue(id) from names group by samevalue(id) having 1=samevalue(id) order by id; -- Postgres error id in order by not in GroupBy
select 1 from names group by samevalue(id) having 1=id; -- Postgres error id in having not in GroupBy
select 1 from names group by samevalue(id) having 1=samevalue(id) order by id; -- Postgres error id in order by not in GroupBy
select samevalue(firstname) from names group by samevalue(id) having 1=samevalue(id) order by samevalue(id); -- Postgres error firstname in select not in GroupBY
select samevalue(firstname) from names group by samevalue(id),firstname having 1=samevalue(id) order by samevalue(id);
select samevalue(id) from names group by 2; -- Postgres error case incorrect location
select samevalue(id) from names group by 1 order by samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select samevalue(id) from names group by 1 having 1!=samevalue(firstname) order by 1; -- Postgres error firstname in Having not in GroupBy
select id from names group by samevalue(id);

-- Using firstname
select samevalue(lastname) from names group by samevalue(lastname) having 1=id; -- Postgres error id in having not in GroupBy
select samevalue(lastname) from names group by samevalue(lastname) having 'Cool'!=samevalue(lastname) order by id; -- Postgres error id in order by not in GroupBy
select 1 from names group by samevalue(lastname) having 1=id; -- Postgres error id in having not in GroupBy
select 1 from names group by samevalue(lastname) having 'Cool'!=samevalue(lastname) order by id; -- Postgres error id in order by not in GroupBy
select samevalue(firstname) from names group by samevalue(lastname) having 'Cool'!=samevalue(lastname) order by samevalue(lastname); -- Postgres error firstname in select not in GroupBY
select samevalue(lastname) from names group by 2; -- Postgres error case incorrect location
select samevalue(lastname) from names group by 1 order by samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select samevalue(lastname) from names group by 1 order by firstname; -- Postgres error firstname in Order by not in GroupBy
select samevalue(lastname) from names group by 1 having 'Zero'!=samevalue(firstname) order by 1; -- Postgres error firstname in Having not in GroupBy
select samevalue(lastname) from names group by 1 having 'Zero'!=firstname order by 1; -- Postgres error firstname in Having not in GroupBy
select firstname from names group by samevalue(firstname);

-- Using Binary and Unary operation along with function operand
select 'test'||samevalue(lastname) from names group by 'test'||samevalue(lastname) having 1=id; -- Postgres error id in having not in GroupBy
select 'test'||samevalue(lastname) from names group by 'test'||samevalue(lastname) having 'testCool'!='test'||samevalue(lastname) order by id; -- Postgres error id in order by not in GroupBy
select 1 from names group by 'test'||samevalue(lastname) having 1=id; -- Postgres error id in having not in GroupBy
select 1 from names group by 'test'||samevalue(lastname) having 'testCool'!='test'||samevalue(lastname) order by id; -- Postgres error id in order by not in GroupBy
select samevalue(firstname) from names group by 'test'||samevalue(lastname) having 'testCool'!='test'||samevalue(lastname) order by 'test'||samevalue(lastname); -- Postgres error firstname in select not in GroupBY
select 'test'||samevalue(lastname) from names group by 2; -- Postgres error case incorrect location
select 'test'||samevalue(lastname) from names group by 1 order by samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select 'test'||samevalue(lastname) from names group by 1 order by 'test'||samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select 'test'||samevalue(lastname) from names group by 1 order by firstname; -- Postgres error firstname in Order by not in GroupBy
select 'test'||samevalue(lastname) from names group by 1 having 'Zero'!=samevalue(firstname) order by 1; -- Postgres error firstname in Having not in GroupBy
select 'test'||samevalue(lastname) from names group by 1 having 'Zero'!=firstname order by 1; -- Postgres error firstname in Having not in GroupBy
select samevalue(id) from names group by 1!=samevalue(id);
select 1!=samevalue(id) from names group by samevalue(firstname);
select 1!=samevalue(id) from names group by not 1!=samevalue(id);
select not 1!=samevalue(id) from names group by not id;


-- Using Binary and Unary operation as function parametersa
select samevalue(NOT 'test'=lastname) from names group by samevalue(NOT 'test'=lastname) having 1=id; -- Postgres error id in having not in GroupBy
select samevalue(NOT 'test'=lastname) from names group by samevalue(NOT 'test'=lastname) having FALSE!=samevalue(NOT 'test'=lastname) order by id; -- Postgres error id in order by not in GroupBy
select samevalue(NOT 'test'=lastname) from names group by samevalue(NOT 'test'=lastname) having FALSE!=NOT 'test'=lastname; -- Postgres error lastname in having not in GroupBy
select 1 from names group by samevalue(NOT 'test'=lastname) having 1=id; -- Postgres error id in having not in GroupBy
select 1 from names group by samevalue(NOT 'test'=lastname) having FALSE!=samevalue(NOT 'test'=lastname) order by id; -- Postgres error id in order by not in GroupBy
select samevalue(firstname) from names group by samevalue(NOT 'test'=lastname) having FALSE!=samevalue(NOT 'test'=lastname) order by samevalue(NOT 'test'=lastname); -- Postgres error firstname in select not in GroupBY
select samevalue(NOT 'test'=lastname) from names group by 2; -- Postgres error case incorrect location
select samevalue(NOT 'test'=lastname) from names group by 1 order by samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select samevalue(NOT 'test'=lastname) from names group by 1 order by 'test'||samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select samevalue(NOT 'test'=lastname) from names group by 1 order by firstname; -- Postgres error firstname in Order by not in GroupBy
select samevalue(NOT 'test'=lastname) from names group by 1 having 'Zero'!=samevalue(firstname) order by 1; -- Postgres error firstname in Having not in GroupBy
select samevalue(NOT 'test'=lastname) from names group by 1 having 'Zero'!=firstname order by 1; -- Postgres error firstname in Having not in GroupBy


-- Parmless function usage
select parmlessfunc() from names group by 1 having parmlessfunc()!=id; -- Postgres error id must be in Group by
select parmlessfunc() from names group by 1 order by parmlessfunc()!=id; -- Postgres error id must be in Group BY
select 1 from names group by lastname having parmlessfunc()!=id; -- Postgres error id must apear in group by
select parmlessfunc()!=firstname from names group by parmlessfunc()!='Success';
select ('Zero'!=lastname)!=(parmlessfunc()!=firstname) from names group by firstname;
select ('Zero'!=lastname)!=(parmlessfunc()!=firstname) from names group by parmlessfunc()!=firstname;


select samevalue(id) from names group by 1,firstname having 1!=samevalue(firstname) order by 1; -- TYPE MISMATCH ERROR

select samevalue(NOT 'test'=lastname) from names group by 1 having 'Zero'!=samevalue(NOT 'test'=lastname) order by 'Zero'!=samevalue(NOT 'test'=lastname); -- TYPE MYSMATCH
select samevalue(NOT 'test'=lastname) from names group by 1 having 'Zero'!=samevalue(NOT 'test'=lastname) order by 1; -- TYPE MYSMATCH
select parmlessfunc() from names group by 1,id having parmlessfunc()!=id; -- TYPE MISMATCH
select parmlessfunc() from names group by 1 having parmlessfunc()!=1; -- TYPE MISMATCH
select parmlessfunc() from names group by 1 order by parmlessfunc()!=1; -- TYPE MISMATCH
select parmlessfunc() from names group by id order by id!=parmlessfunc(); -- TYPE MISTMATCH
select 1 from names group by id having parmlessfunc()!=id; -- TYPE MISMATCH
select 1 from names group by id having paramlessfunc()!=lastname order by parmlessfunc(); -- lastname not in group by

-- function_call_STATEMENT with CAST spec and CAST expr
-- function with CAST_EXPRESSION
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS INTEGER)); -- Postgres error saying id must be in GroupBy
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS INTEGER)), samevalue(id); -- Postgres error saying id must be in GroupBy
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS NUMERIC)) having 1=samevalue(CAST(id AS INTEGER)); -- Postgres error saying id must be in GroupBY
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS NUMERIC)) having 1=samevalue(CAST(id AS NUMERIC)) order by samevalue(CAST(id AS INTEGER)); -- Postgres error saying id must be in GroupBy
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS NUMERIC)) having 1=id; -- Postgres error id in having not in GroupBy
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS NUMERIC)) having 1=samevalue(CAST(id AS NUMERIC)) order by id; -- Postgres error id in order by not in GroupBy
select 1 from names group by samevalue(CAST(id AS NUMERIC)) having 1=id; -- Postgres error id in having not in GroupBy
select 1 from names group by samevalue(CAST(id AS NUMERIC)) having 1=samevalue(CAST(id AS NUMERIC)) order by id; -- Postgres error id in order by not in GroupBy
select samevalue(firstname) from names group by samevalue(CAST(id AS NUMERIC)) having 1=samevalue(CAST(id AS NUMERIC)) order by samevalue(CAST(id AS NUMERIC)); -- Postgres error firstname in select not in GroupBY
select samevalue(CAST(id AS NUMERIC)) from names group by 2; -- Postgres error case incorrect location
select samevalue(CAST(id AS NUMERIC)) from names group by 1 order by samevalue(CAST(id AS INTEGER)); -- Postgres error id not in GroupBy
select samevalue(CAST(id AS NUMERIC)) from names group by 1 order by samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select samevalue(CAST(id AS NUMERIC)) from names group by 1 having 1!=samevalue(firstname) order by 1; -- Postgres error firstname in Having not in GroupBy
select samevalue(CAST(id AS NUMERIC)) from names group by 1,firstname having 1!=samevalue(firstname) order by 1; -- TYPE mismatch

-- function with CAST_SPECIFICATION
select samevalue(id::NUMERIC) from names group by samevalue(id::INTEGER); -- Postgres error saying id must be in GroupBy
select samevalue(id::NUMERIC) from names group by samevalue(id::INTEGER), samevalue(id); -- Postgres error saying id must be in GroupBy
select samevalue(id::NUMERIC) from names group by samevalue(id::NUMERIC) having 1=samevalue(id::INTEGER); -- Postgres error saying id must be in GroupBY
select samevalue(id::NUMERIC) from names group by samevalue(id::NUMERIC) having 1=samevalue(id::NUMERIC) order by samevalue(id::INTEGER); -- Postgres error saying id must be in GroupBy
select samevalue(id::NUMERIC) from names group by samevalue(id::NUMERIC) having 1=id; -- Postgres error id in having not in GroupBy
select samevalue(id::NUMERIC) from names group by samevalue(id::NUMERIC) having 1=samevalue(id::NUMERIC) order by id; -- Postgres error id in order by not in GroupBy
select 1 from names group by samevalue(id::NUMERIC) having 1=id; -- Postgres error id in having not in GroupBy
select 1 from names group by samevalue(id::NUMERIC) having 1=samevalue(id::NUMERIC) order by id; -- Postgres error id in order by not in GroupBy
select samevalue(firstname) from names group by samevalue(id::NUMERIC) having 1=samevalue(id::NUMERIC) order by samevalue(id::NUMERIC); -- Postgres error firstname in select not in GroupBY
select samevalue(id::NUMERIC) from names group by 2; -- Postgres error case incorrect location
select samevalue(id::NUMERIC) from names group by 1 order by samevalue(id::INTEGER); -- Postgres error id not in GroupBy
select samevalue(id::NUMERIC) from names group by 1 order by samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select samevalue(id::NUMERIC) from names group by 1 having 1!=samevalue(firstname) order by 1; -- Postgres error firstname in Having not in GroupBy
select samevalue(id::NUMERIC) from names group by 1,firstname having 1!=samevalue(firstname) order by 1; -- Type mismatch error

-- With CAST_EXPRESSION usage
select samevalue(firstname) from names group by CAST(id AS NUMERIC) having 1=CAST(id AS NUMERIC) order by CAST(id AS NUMERIC); -- Postgres error firstname in select not in GroupBY
select CAST(id AS NUMERIC) from names group by 1 order by samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select CAST(id AS NUMERIC) from names group by 1 having 1!=samevalue(firstname) order by 1; -- Postgres error firstname in Having not in GroupBy

-- With CAST_SPECIFICATION
select samevalue(firstname) from names group by id::NUMERIC having 1=id::NUMERIC order by id::NUMERIC; -- Postgres error firstname in select not in GroupBY
select id::NUMERIC from names group by 1 order by samevalue(firstname); -- Postgres error firstname in Order by not in GroupBy
select id::NUMERIC from names group by 1 having 1!=samevalue(firstname) order by 1; -- Postgres error firstname in Having not in GroupBy

-- Using Binary and Unary operation along with function operand and cast speci
select 1+samevalue(id)::INTEGER from names group by 1+samevalue(id)::NUMERIC; -- Postgres error id not in GroupBY
select 1+samevalue(id)::INTEGER from names group by 1 having 1+samevalue(id)::NUMERIC != 2; -- Postgres error id not in GroupBy
select 1+CAST(samevalue(id) as INTEGER) from names group by 1+CAST(samevalue(id) as NUMERIC); -- Postgres error id not in Groupby

