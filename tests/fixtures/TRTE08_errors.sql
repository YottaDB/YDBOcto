#################################################################
#                                                               #
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.       #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

-- TRTE08 : OCTO972 : Verify that LIKE and SIMILAR TO issue error when operands are non-STRING or non-NULL type
-- LIKE
-- Integer
select * from names where id like 1;
select * from names where id like 4::integer;
select * from names where 1 like id;
select * from names where 4::integer like id;
select * from names order by id like 1;
select * from names where id like (select id);
select * from names where (select id) like id;
select * from names where id like (select id union select id);
select * from names where (select id union select id) like id;
select * from names where id like (VALUES(id));
select * from names where (VALUES(id)) like id;
select * from names where id like (select 'test');
select * from names where (select 'test') like id;
select * from names where id like (select 'test' union select 'test');
select * from names where (select 'test' union select 'test') like id;
select * from names where id like (VALUES('test'));
select * from names where (VALUES('test')) like id;
select * from names where id like 'test';
select * from names where id + id like 'test';
select * from names where 'test' like id + id;
select * from names where id like (select NULL);
select * from names where (select NULL) like id;
select * from names where id like (select NULL union select NULL);
select * from names where (select NULL union select NULL) like id;
select * from names where id like (VALUES(NULL));
select * from names where (VALUES(NULL)) like id;
select * from names where id like NULL;
select * from names where id + id like NULL;
select * from names where NULL like id + id;
-- Numeric
select * from names where id::numeric like 1.1;
select * from names where id::numeric like 4.1::numeric;
select * from names where 1.1 like id::numeric;
select * from names where 4.1::numeric like id::numeric;
select * from names order by id::numeric like 1.1;
select * from names where id::numeric like (select 1.1);
select * from names where (select 1.1::numeric) like 1.1;
select * from names where id::numeric like (select id::numeric union select id::numeric);
select * from names where (select id::numeric union select id::numeric) like id::numeric;
select * from names where id::numeric like (VALUES(id::numeric));
select * from names where (VALUES(id::numeric)) like id::numeric;
select * from names where id::numeric like (select 'test');
select * from names where (select 'test') like id::numeric;
select * from names where id::numeric like (select 'test' union select 'test');
select * from names where (select 'test' union select 'test') like id::numeric;
select * from names where id::numeric like (VALUES('test'));
select * from names where (VALUES('test')) like id::numeric;
select * from names where id::numeric like 'test';
select * from names where id::numeric + id::numeric like 'test';
select * from names where 'test' like id::numeric + id::numeric;
select * from names where id::numeric like (select NULL);
select * from names where (select NULL) like id::numeric;
select * from names where id::numeric like (select NULL union select NULL);
select * from names where (select NULL union select NULL) like id::numeric;
select * from names where id::numeric like (VALUES(NULL));
select * from names where (VALUES(NULL)) like id::numeric;
select * from names where id::numeric like NULL;
select * from names where id::numeric + id::numeric like NULL;
select * from names where NULL like id::numeric + id::numeric;
-- Boolean
select * from names where id::boolean like TRUE;
select * from names where id::boolean like 1::boolean;
select * from names where 1 like id::boolean;
select * from names where 4::boolean like id::boolean;
select * from names order by id::boolean like 1;
select * from names where id::boolean like (select id::boolean);
select * from names where (select id::boolean) like id::boolean;
select * from names where id::boolean like (select id::boolean union select id::boolean);
select * from names where (select id::boolean union select id::boolean) like id::boolean;
select * from names where id::boolean like (VALUES(id::boolean));
select * from names where (VALUES(id::boolean)) like id::boolean;
select * from names where id::boolean like (select 'test');
select * from names where (select 'test') like id::boolean;
select * from names where id::boolean like (select 'test' union select 'test');
select * from names where (select 'test' union select 'test') like id::boolean;
select * from names where id::boolean like (VALUES('test'));
select * from names where (VALUES('test')) like id::boolean;
select * from names where id::boolean like 'test';
select * from names where id::boolean like (select NULL);
select * from names where (select NULL) like id::boolean;
select * from names where id::boolean like (select NULL union select NULL);
select * from names where (select NULL union select NULL) like id::boolean;
select * from names where id::boolean like (VALUES(NULL));
select * from names where (VALUES(NULL)) like id::boolean;
select * from names where id::boolean like NULL;
