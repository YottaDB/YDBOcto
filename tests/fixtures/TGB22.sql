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

-- Postgres function
-- create function paramlessfunc() returns integer
-- as 'select 1;'
-- LANGUAGE SQL
-- IMMUTABLE;


-- Functions in GroupBy
-- Using id
select samevalue(id) from names;
select samevalue(id) from names group by id;
select samevalue(id) from names group by samevalue(id);
select samevalue(id) from names group by id having 1=samevalue(id);
select samevalue(id) from names group by id having 1=samevalue(id) order by samevalue(id);
select samevalue(id) from names group by id having 1=id;
select samevalue(id) from names group by id having 1=samevalue(id) order by id;
select samevalue(id) from names group by samevalue(id) having 1=samevalue(id);
select samevalue(id) from names group by samevalue(id) having 1=samevalue(id) order by samevalue(id);
select 1 from names group by id having 1=samevalue(id);
select 1 from names group by id having 1=samevalue(id) order by samevalue(id);
select 1 from names group by id having 1=id;
select 1 from names group by id having 1=samevalue(id) order by id;
select 1 from names group by samevalue(id) having 1=samevalue(id);
select 1 from names group by samevalue(id) having 1=samevalue(id) order by samevalue(id);
select samevalue(firstname) from names group by samevalue(id),firstname having 1=samevalue(id) order by samevalue(id);
select samevalue(id) from names group by 1;
select samevalue(id) from names group by 1 order by samevalue(id);
select samevalue(id) from names group by 1 having 1!=samevalue(id) order by samevalue(id);
select samevalue(id) from names group by 1 order by 1;
select samevalue(id) from names group by 1 having 1!=samevalue(id) order by 1;
select samevalue(id) from names group by 1,firstname order by samevalue(firstname);
select samevalue(id) from names group by samevalue(id),id;

-- Using firstname
select samevalue(lastname) from names;
select samevalue(lastname) from names group by lastname;
select samevalue(lastname) from names group by samevalue(lastname);
select samevalue(lastname) from names group by lastname having 'Cool'!=samevalue(lastname);
select samevalue(lastname) from names group by lastname having 'Cool'!=samevalue(lastname) order by samevalue(lastname);
select samevalue(lastname) from names group by lastname having 'Cool'!=lastname;
select samevalue(lastname) from names group by lastname having 'Cool'!=samevalue(lastname) order by lastname;
select samevalue(lastname) from names group by samevalue(lastname) having 'Cool'!=samevalue(lastname);
select samevalue(lastname) from names group by samevalue(lastname) having 'Cool'!=samevalue(lastname) order by samevalue(lastname);
select 1 from names group by lastname having 'Cool'!=samevalue(lastname);
select 1 from names group by lastname having 'Cool'!=samevalue(lastname) order by samevalue(lastname);
select 1 from names group by lastname having 'Cool'!=lastname;
select 1 from names group by lastname having 'Cool'!=samevalue(lastname) order by lastname;
select 1 from names group by samevalue(lastname) having 'Cool'!=samevalue(lastname);
select 1 from names group by samevalue(lastname) having 'Cool'!=samevalue(lastname) order by samevalue(lastname);
select samevalue(firstname) from names group by samevalue(lastname),firstname having 'Cool'!=samevalue(lastname) order by samevalue(lastname);
select samevalue(lastname) from names group by 1;
select samevalue(lastname) from names group by 1 order by samevalue(lastname);
select samevalue(lastname) from names group by 1 having 'Zero'!=samevalue(lastname) order by samevalue(lastname);
select samevalue(lastname) from names group by 1 order by 1;
select samevalue(lastname) from names group by 1 having 'Zero'!=samevalue(lastname) order by 1;
select samevalue(lastname) from names group by 1,firstname order by samevalue(firstname);
select samevalue(lastname) from names group by 1,firstname having 'Zero'!=samevalue(firstname) order by 1;
select samevalue(lastname) from names group by 1,firstname having 'Zero'!=firstname order by 1;
select samevalue(lastname) from names group by samevalue(lastname),id;

-- Using Binary and Unary operation along with function operand
select 'test'||samevalue(lastname) from names;
select 'test'||samevalue(lastname) from names group by lastname;
select 'test'||samevalue(lastname) from names group by 'test'||samevalue(lastname);
select 'test'||samevalue(lastname) from names group by lastname having 'testCool'!='test'||samevalue(lastname);
select 'test'||samevalue(lastname) from names group by lastname having 'testCool'!='test'||samevalue(lastname) order by 'test'||samevalue(lastname);
select 'test'||samevalue(lastname) from names group by lastname having 'Cool'!=lastname;
select 'test'||samevalue(lastname) from names group by lastname having 'testCool'!='test'||samevalue(lastname) order by lastname;
select 'test'||samevalue(lastname) from names group by 'test'||samevalue(lastname) having 'testCool'!='test'||samevalue(lastname);
select 'test'||samevalue(lastname) from names group by 'test'||samevalue(lastname) having 'testCool'!='test'||samevalue(lastname) order by 'test'||samevalue(lastname);
select 1 from names group by lastname having 'testCool'!='test'||samevalue(lastname);
select 1 from names group by lastname having 'testCool'!='test'||samevalue(lastname) order by 'test'||samevalue(lastname);
select 1 from names group by lastname having 'Cool'!=lastname;
select 1 from names group by lastname having 'testCool'!='test'||samevalue(lastname) order by lastname;
select 1 from names group by 'test'||samevalue(lastname) having 'testCool'!='test'||samevalue(lastname);
select 1 from names group by 'test'||samevalue(lastname) having 'testCool'!='test'||samevalue(lastname) order by 'test'||samevalue(lastname);
select samevalue(firstname) from names group by 'test'||samevalue(lastname),firstname having 'testCool'!='test'||samevalue(lastname) order by 'test'||samevalue(lastname);
select 'test'||samevalue(lastname) from names group by 1;
select 'test'||samevalue(lastname) from names group by 1 order by 'test'||samevalue(lastname);
select 'test'||samevalue(lastname) from names group by 1 having 'Zero'!='test'||samevalue(lastname) order by 'test'||samevalue(lastname);
select 'test'||samevalue(lastname) from names group by 1 order by 1;
select 'test'||samevalue(lastname) from names group by 1 having 'Zero'!='test'||samevalue(lastname) order by 1;
select 'test'||samevalue(lastname) from names group by 1,firstname order by samevalue(firstname);
select 'test'||samevalue(lastname) from names group by 1,firstname having 'Zero'!=samevalue(firstname) order by 1;
select 'test'||samevalue(lastname) from names group by 1,firstname having 'Zero'!=firstname order by 1;
select 'test'||samevalue(lastname) from names group by 'test'||samevalue(lastname),id;
select not 1!=samevalue(id) from names group by not 1!=samevalue(id);
select not 1!=samevalue(id) from names group by id;
select samevalue('test'||samevalue(lastname)) from names group by lastname;
select samevalue('test'||samevalue(lastname)) from names group by samevalue(lastname) order by 'zero'||samevalue(lastname);
select samevalue('test'||samevalue(lastname)) from names group by 'test'||samevalue(lastname);
select samevalue('test'||samevalue(lastname)) from names group by samevalue('test'||samevalue(lastname));


-- Using Binary and Unary operation as function parametersa
select samevalue(NOT 'test'=lastname) from names;
select samevalue(NOT 'test'=lastname) from names group by lastname;
select samevalue(NOT 'test'=lastname) from names group by 'test'=lastname;
select samevalue(NOT 'test'=lastname) from names group by NOT 'test'=lastname;
select samevalue(NOT 'test'=lastname) from names group by samevalue(NOT 'test'=lastname);
select samevalue(NOT 'test'=lastname) from names group by lastname having FALSE!=samevalue(NOT 'test'=lastname);
select samevalue(NOT 'test'=lastname) from names group by lastname having FALSE!=samevalue(NOT 'test'=lastname) order by samevalue(NOT 'test'=lastname);
select samevalue(NOT 'test'=lastname) from names group by lastname having 'Cool'!=lastname;
select samevalue(NOT 'test'=lastname) from names group by lastname having FALSE!=samevalue(NOT 'test'=lastname) order by lastname;
select samevalue(NOT 'test'=lastname) from names group by samevalue(NOT 'test'=lastname) having FALSE!=samevalue(NOT 'test'=lastname);
select samevalue(NOT 'test'=lastname) from names group by samevalue(NOT 'test'=lastname) having FALSE!=samevalue(NOT 'test'=lastname) order by samevalue(NOT 'test'=lastname);
select samevalue(NOT 'test'=lastname) from names group by NOT 'test'=lastname having FALSE!=(NOT 'test'=lastname);
select 1 from names group by lastname having FALSE!=samevalue(NOT 'test'=lastname);
select 1 from names group by lastname having FALSE!=samevalue(NOT 'test'=lastname) order by 'test'=lastname;
select 1 from names group by lastname having 'Cool'!=lastname;
select 1 from names group by lastname having FALSE!=samevalue(NOT 'test'=lastname) order by lastname;
select 1 from names group by samevalue(NOT 'test'=lastname) having FALSE!=samevalue(NOT 'test'=lastname);
select 1 from names group by samevalue(NOT 'test'=lastname) having FALSE!=samevalue(NOT 'test'=lastname) order by samevalue(NOT 'test'=lastname);
select samevalue(firstname) from names group by samevalue(NOT 'test'=lastname),firstname having FALSE!=samevalue(NOT 'test'=lastname) order by samevalue(NOT 'test'=lastname);
select samevalue(NOT 'test'=lastname) from names group by 1;
select samevalue(NOT 'test'=lastname) from names group by 1 order by samevalue(NOT 'test'=lastname);
select samevalue(NOT 'test'=lastname) from names group by 1 order by 1;
select samevalue(NOT 'test'=lastname) from names group by 1,firstname order by samevalue(firstname);
select samevalue(NOT 'test'=lastname) from names group by 1,firstname having 'Zero'!=samevalue(firstname) order by 1;
select samevalue(NOT 'test'=lastname) from names group by 1,firstname having 'Zero'!=firstname order by 1;
select samevalue(NOT 'test'=lastname) from names group by samevalue(NOT 'test'=lastname),id;
select not TRUE!=samevalue(NOT 'test'=lastname) from names group by not TRUE!=samevalue(NOT 'test'=lastname);
select not TRUE!=samevalue(NOT 'test'=lastname) from names group by lastname;
select not TRUE!=samevalue(NOT 'test'=lastname) from names group by samevalue(NOT 'test'=lastname);
select not TRUE!=samevalue(NOT 'test'=lastname) from names group by TRUE!=samevalue(NOT 'test'=lastname);


-- Parmless function usage
select 1 from names group by parmlessfunc();
select firstname from names group by parmlessfunc(),firstname;
select parmlessfunc() from names group by parmlessfunc(),firstname;
select parmlessfunc(),firstname from names group by parmlessfunc(),firstname;
select parmlessfunc() from names group by parmlessfunc();
select parmlessfunc() from names group by lastname;
select parmlessfunc() from names group by parmlessfunc(),id;
select parmlessfunc()!='Success' from names group by parmlessfunc()!='Success';
select parmlessfunc()!=firstname from names group by parmlessfunc()!=firstname;
select parmlessfunc()!=firstname from names group by firstname;
select parmlessfunc()!=firstname from names group by parmlessfunc()!=firstname,firstname;
select ('Zero'!=lastname)!=(parmlessfunc()!=firstname) from names group by firstname,lastname;
select ('Zero'!=lastname)!=(parmlessfunc()!=firstname) from names group by ('Zero'!=lastname)!=(parmlessfunc()!=firstname);
select parmlessfunc() from names group by 1;
select parmlessfunc() from names group by 1 order by 1;
select parmlessfunc() from names group by parmlessfunc() order by 1;
select parmlessfunc() from names group by 1,firstname having parmlessfunc()!=firstname;
select parmlessfunc() from names group by 1 having parmlessfunc()!='Zero';
select parmlessfunc() from names group by 1 order by parmlessfunc();
select parmlessfunc() from names group by 1 order by parmlessfunc()!='Zero';
select parmlessfunc() from names group by lastname;
select parmlessfunc() from names group by lastname order by lastname!=parmlessfunc();
select 1 from names group by parmlessfunc();
select 1 from names group by parmlessfunc() order by parmlessfunc();
select 1 from names group by lastname order by parmlessfunc();
select 1 from names group by lastname having parmlessfunc()!=lastname;
select 1 from names group by lastname having parmlessfunc()!=lastname order by parmlessfunc();
select parmlessfunc() from names group by lastname order by lastname!=parmlessfunc();
select parmlessfunc() from names group by lastname having lastname!=parmlessfunc();

-- function_call_STATEMENT with CAST spec and CAST expr
-- function with CAST_EXPRESSION
select samevalue(CAST(id AS NUMERIC)) from names;
select samevalue(CAST(id AS NUMERIC)) from names group by id;
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS NUMERIC));
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS INTEGER)),id;
select samevalue(CAST(id AS NUMERIC)) from names group by id having 1=samevalue(CAST(id AS NUMERIC));
select samevalue(CAST(id AS NUMERIC)) from names group by id having 1=samevalue(CAST(id AS INTEGER));
select samevalue(CAST(id AS NUMERIC)) from names group by id having 1=samevalue(CAST(id AS NUMERIC)) order by samevalue(CAST(id AS NUMERIC));
select samevalue(CAST(id AS NUMERIC)) from names group by id having 1=id;
select samevalue(CAST(id AS NUMERIC)) from names group by id having 1=samevalue(CAST(id AS NUMERIC)) order by id;
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS NUMERIC)) having 1=samevalue(CAST(id AS NUMERIC));
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS NUMERIC)) having 1=samevalue(CAST(id AS NUMERIC)) order by samevalue(CAST(id AS NUMERIC));
select 1 from names group by id having 1=samevalue(CAST(id AS NUMERIC));
select 1 from names group by id having 1=samevalue(CAST(id AS NUMERIC)) order by samevalue(CAST(id AS NUMERIC));
select 1 from names group by id having 1=id;
select 1 from names group by id having 1=samevalue(CAST(id AS NUMERIC)) order by id;
select 1 from names group by samevalue(CAST(id AS NUMERIC)) having 1=samevalue(CAST(id AS NUMERIC));
select 1 from names group by samevalue(CAST(id AS NUMERIC)) having 1=samevalue(CAST(id AS NUMERIC)) order by samevalue(CAST(id AS NUMERIC));
select samevalue(firstname) from names group by samevalue(CAST(id AS NUMERIC)),firstname having 1=samevalue(CAST(id AS NUMERIC)) order by samevalue(CAST(id AS NUMERIC));
select samevalue(CAST(id AS NUMERIC)) from names group by 1;
select samevalue(CAST(id AS NUMERIC)) from names group by 1 order by samevalue(CAST(id AS NUMERIC));
select samevalue(CAST(id AS NUMERIC)) from names group by 1 having 1!=samevalue(CAST(id AS NUMERIC)) order by samevalue(CAST(id AS NUMERIC));
select samevalue(CAST(id AS NUMERIC)) from names group by 1 order by 1;
select samevalue(CAST(id AS NUMERIC)) from names group by 1 having 1!=samevalue(CAST(id AS NUMERIC)) order by 1;
select samevalue(CAST(id AS NUMERIC)) from names group by 1,firstname order by samevalue(firstname);
select samevalue(CAST(id AS NUMERIC)) from names group by 1,firstname having 'Zero'!=samevalue(firstname) order by 1;
select samevalue(CAST(id AS NUMERIC)) from names group by samevalue(CAST(id AS NUMERIC)),id;

-- function with CAST_SPECIFICATION
select samevalue(id::NUMERIC) from names;
select samevalue(id::NUMERIC) from names group by id;
select samevalue(id::NUMERIC) from names group by samevalue(id::NUMERIC);
select samevalue(id::NUMERIC) from names group by samevalue(id::INTEGER),id;
select samevalue(id::NUMERIC) from names group by id having 1=samevalue(id::NUMERIC);
select samevalue(id::NUMERIC) from names group by id having 1=samevalue(id::INTEGER);
select samevalue(id::NUMERIC) from names group by id having 1=samevalue(id::NUMERIC) order by samevalue(id::NUMERIC);
select samevalue(id::NUMERIC) from names group by id having 1=id;
select samevalue(id::NUMERIC) from names group by id having 1=samevalue(id::NUMERIC) order by id;
select samevalue(id::NUMERIC) from names group by samevalue(id::NUMERIC) having 1=samevalue(id::NUMERIC);
select samevalue(id::NUMERIC) from names group by samevalue(id::NUMERIC) having 1=samevalue(id::NUMERIC) order by samevalue(id::NUMERIC);
select 1 from names group by id having 1=samevalue(id::NUMERIC);
select 1 from names group by id having 1=samevalue(id::NUMERIC) order by samevalue(id::NUMERIC);
select 1 from names group by id having 1=id;
select 1 from names group by id having 1=samevalue(id::NUMERIC) order by id;
select 1 from names group by samevalue(id::NUMERIC) having 1=samevalue(id::NUMERIC);
select 1 from names group by samevalue(id::NUMERIC) having 1=samevalue(id::NUMERIC) order by samevalue(id::NUMERIC);
select samevalue(firstname) from names group by samevalue(id::NUMERIC),firstname having 1=samevalue(id::NUMERIC) order by samevalue(id::NUMERIC);
select samevalue(id::NUMERIC) from names group by 1;
select samevalue(id::NUMERIC) from names group by 1 order by samevalue(id::NUMERIC);
select samevalue(id::NUMERIC) from names group by 1 having 1!=samevalue(id::NUMERIC) order by samevalue(id::NUMERIC);
select samevalue(id::NUMERIC) from names group by 1 order by 1;
select samevalue(id::NUMERIC) from names group by 1 having 1!=samevalue(id::NUMERIC) order by 1;
select samevalue(id::NUMERIC) from names group by 1,firstname order by samevalue(firstname);
select samevalue(id::NUMERIC) from names group by 1,firstname having 'Zero'!=samevalue(firstname) order by 1;
select samevalue(id::NUMERIC) from names group by samevalue(id::NUMERIC),id;

-- With CAST_EXPRESSION usage
select samevalue(firstname) from names group by CAST(id AS NUMERIC),firstname having 1=CAST(id AS NUMERIC) order by CAST(id AS NUMERIC);
select CAST(id AS NUMERIC) from names group by 1,firstname order by samevalue(firstname);
select CAST(id AS NUMERIC) from names group by 1,firstname having 'Zero'!=samevalue(firstname) order by 1;

-- With CAST_SPECIFICATION
select samevalue(firstname) from names group by id::NUMERIC,firstname having 1=id::NUMERIC order by id::NUMERIC;
select id::NUMERIC from names group by 1,firstname order by samevalue(firstname);
select id::NUMERIC from names group by 1,firstname having 'Zero'!=samevalue(firstname) order by 1;

-- Using Binary and Unary operation along with function operand and cast speci
select 1+samevalue(id)::INTEGER from names group by 1+samevalue(id)::INTEGER;
select 1+samevalue(id)::INTEGER from names group by 1;
select 1+samevalue(id)::INTEGER from names group by 1 having 1+samevalue(id)::INTEGER != 2;
select 1+CAST(samevalue(id) as INTEGER) from names group by 1+CAST(samevalue(id) as INTEGER);

select parmlessfunc() from names group by parmlessfunc()::NUMERIC;
select parmlessfunc() from names group by parmlessfunc()::NUMERIC having 1!=parmlessfunc()::NUMERIC;
select parmlessfunc() from names group by parmlessfunc()::NUMERIC having 1!=parmlessfunc()::INTEGER;
select parmlessfunc() from names group by 1 having 1!=parmlessfunc()::INTEGER;
select parmlessfunc() from names group by 1 having 1!=parmlessfunc()::NUMERIC;
select parmlessfunc() from names group by CAST(parmlessfunc() as NUMERIC);
select parmlessfunc() from names group by CAST(parmlessfunc() as NUMERIC) having 1!=CAST(parmlessfunc() as NUMERIC);
select parmlessfunc() from names group by CAST(parmlessfunc() as NUMERIC) having 1!=CAST(parmlessfunc() as INTEGER);
select parmlessfunc() from names group by 1 having 1!=CAST(parmlessfunc() as INTEGER);
select parmlessfunc() from names group by 1 having 1!=CAST(parmlessfunc() as NUMERIC);

