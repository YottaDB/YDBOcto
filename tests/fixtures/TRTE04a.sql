#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select id from names where firstname ~ ('abcd' || 'efgh');
-- below query is different than the one above because one row of names table has an empty string. reg_match behavior with empty string is tested by the below query.
select id from names where lastname ~ ('abcd' || 'efgh');
select id from names where firstname ~ ('Zero|Lord');
select id from names where firstname ~ ('Ze' || 'ro');
select id from names where firstname ~ ('e' || 'ro');
select id from names where firstname LIKE ('e' || 'ro');
select firstname from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where firstname SIMILAR TO (firstname || lastname);
select firstname from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where firstname LIKE (firstname || lastname);
select * from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where (firstname || lastname) SIMILAR TO firstname;
select * from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where (firstname || lastname) LIKE firstname;
select * from ((select * from names) union (select 8 as id,'ey' as firstname, '' as lastname))n1 where (firstname || lastname) ~ firstname;
select id from names where firstname SIMILAR TO ('e' || 'ro');
select id from names where firstname LIKE ('e' || 'ro');
select * from names where id::varchar SIMILAR TO (3*id)::varchar;
select * from names where id::varchar LIKE (3*id)::varchar;
select * from names where lastname SIMILAR TO lastname || lastname;
select * from names where lastname LIKE lastname || lastname;
select * from names where firstname SIMILAR TO lastname || 'abcd';
select * from names where firstname LIKE lastname || 'abcd';
select id from names where 'abd' SIMILAR TO 'abc|d';
select id from names where 'abd' LIKE 'abc|d';
select id from names where firstname LIKE lastname;

-- sub queries
select id from names where 'hello' ~~ (select * from (select 'hello' as col1) u1)::varchar;
select id from names where 'hello' ~~ (select col1 from (select 'hello' as col1) u1)::varchar;
select id from names where 'hello' ~~ (select * from (select 'helalo' as col1) u1)::varchar;
select id from names where 'hello' ~~ (select col1 from ((select 'hello' as col1,1 as col2) union (select 'temp' as col1, 2 as col2)) u1 limit 1)::varchar;

-- queries which verify octo's regex processing when column value has regex meta characters
select * from ((select * from names) union (select 8 as id,'ey' as firstname, 'e%' as lastname))n1 where firstname SIMILAR TO lastname;
select * from ((select * from names) union (select 8 as id,'ey' as firstname, 'e\\%' as lastname))n1 where firstname LIKE lastname;
select * from ((select * from names) union (select 8 as id,'ey' as firstname, 'e%' as lastname))n1 where firstname LIKE lastname;
select firstname from ((select * from names) union (select 8 as id,'ey' as firstname, '%' as lastname))n1 where firstname SIMILAR TO ('J' || lastname);

-- queries which verify octo's regex processing when regex meta characters are present in pattern expression
select * from names where firstname like 'J' || lastname || '%';
select * from names where firstname SIMILAR TO 'J' || lastname || '%';
select firstname from ((select * from names) union (select 8 as id,'ey' as firstname, '%' as lastname))n1 where firstname SIMILAR TO ('J' || '%');
select firstname from ((select * from names) union (select 8 as id,'ey' as firstname, '%1%' as lastname))n1 where (id*3)::varchar SIMILAR TO lastname;

-- queries which have NULL as regex operation input
select * from names where lastname LIKE NULL;
select * from names where lastname SIMILAR TO NULL;
select * from names where lastname ~ NULL;
select * from names where NULL LIKE 'abc';
select * from names where NULL ~ 'abc';
select * from names where NULL SIMILAR TO 'abc';
select * from names where lastname NOT LIKE NULL;
select * from names where lastname NOT SIMILAR TO NULL;
select * from names where lastname !~ NULL;
select * from names where NULL NOT LIKE 'abc';
select * from names where NULL NOT SIMILAR TO 'abc';
select * from names where NULL !~ 'abc';
select * from names where firstname LIKE 'abc'||NULL;
select * from names where firstname SIMILAR TO 'abc'||NULL;
