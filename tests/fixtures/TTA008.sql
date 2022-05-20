#################################################################
#                                                               #
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.  #
# All rights reserved.                                          #
#                                                               #
#       This source code contains the intellectual property     #
#       of its copyright holder(s), and is made available       #
#       under a license.  If you do not know the terms of       #
#       the license, please stop and do not read further.       #
#                                                               #
#################################################################

select coalesce(n1.*) from names n1 group by coalesce(n1.*);
select coalesce(n1.*) from names n1 group by coalesce(n1.*) having coalesce(n1.*)!=NULL;
select coalesce(n1.*),n1.* from names n1 group by 2 having coalesce(n1.*)!=NULL;
select coalesce(n1.*),n1.* from names n1 group by 1 having coalesce(n1.*)!=NULL; -- Postgres error n1.id in select list should be in groupby
select coalesce(n1.*) from names n1 group by coalesce(n1.*) having coalesce(n1.*)!=NULL order by coalesce(n1.*);
select coalesce(n1.*) from names n1 group by coalesce(n1.*) order by coalesce(n1.*);
select coalesce(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id;
select coalesce(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by coalesce(n1.*); -- Postgres n1.* in select list should be in greoup by
select coalesce(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by coalesce(n2.*); -- Postgres n1.* in select list should be in greoup by
select coalesce(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by coalesce(n1.*,n2.*);
select coalesce(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by coalesce(n2.*,n1.*); -- Postgres errro select list n1.* must be in groupby
select coalesce(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by 1 order by 1;
select 1 from names n1 join names n2 on n1.id=n2.id group by 1 order by coalesce(n1.*,n2.*); -- Postgres error order by n1.* must appear in groupby
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.* order by coalesce(n1.*,n2.*); -- Postgres error order by n2.* must appear in groupby
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.*,n2.* order by coalesce(n1.*,n2.*);
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.*,coalesce(n1.*,n2.*),n2.* order by coalesce(n1.*),coalesce(n1.*,n2.*);

select nullif(n1.*) from names n1; -- Postgres error
select nullif(n1.*) from names n1 group by nullif(n1.*); -- Postgres error
select nullif(n1.*) from names n1 group by nullif(n1.*) having nullif(n1.*)!=NULL; --Postgres error
select nullif(n1.*),n1.* from names n1 group by 2 having nullif(n1.*)!=NULL order by nullif(n1.*); -- Posgres error
select nullif(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id;
select nullif(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by nullif(n1.*,n2.*);
select nullif(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by nullif(n2.*,n1.*); -- Postgres errro select list n1.* must be in groupby
select nullif(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by 1 order by 1;
select 1 from names n1 join names n2 on n1.id=n2.id group by 1 order by nullif(n1.*,n2.*); -- Postgres error order by n1.* must appear in groupby
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.* order by nullif(n1.*,n2.*); -- Postgres error order by n2.* must appear in groupby
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.*,n2.* order by nullif(n1.*,n2.*);
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.*,nullif(n1.*,n2.*),n2.* order by greatest(n1.*),nullif(n1.*,n2.*);


select greatest(n1.*) from names n1;
select greatest(n1.*) from names n1 group by greatest(n1.*);
select greatest(n1.*) from names n1 group by greatest(n1.*) having greatest(n1.*)!=NULL;
select greatest(n1.*),n1.* from names n1 group by 2 having greatest(n1.*)!=NULL;
select greatest(n1.*),n1.* from names n1 group by 1 having greatest(n1.*)!=NULL; -- Postgres error n1.id in select list should be in groupby
select greatest(n1.*) from names n1 group by greatest(n1.*) having greatest(n1.*)!=NULL order by greatest(n1.*);
select greatest(n1.*) from names n1 group by greatest(n1.*) order by greatest(n1.*);
select greatest(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id;
select greatest(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by greatest(n1.*); -- Postgres n1.* in select list should be in greoup by
select greatest(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by greatest(n2.*); -- Postgres n1.* in select list should be in greoup by
select greatest(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by greatest(n1.*,n2.*);
select greatest(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by greatest(n2.*,n1.*); -- Postgres errro select list n1.* must be in groupby
select greatest(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by 1 order by 1;
select 1 from names n1 join names n2 on n1.id=n2.id group by 1 order by greatest(n1.*,n2.*); -- Postgres error order by n1.* must appear in groupby
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.* order by greatest(n1.*,n2.*); -- Postgres error order by n2.* must appear in groupby
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.*,n2.* order by greatest(n1.*,n2.*);
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.*,greatest(n1.*,n2.*),n2.* order by greatest(n1.*),greatest(n1.*,n2.*);

select least(n1.*) from names n1;
select least(n1.*) from names n1 group by least(n1.*);
select least(n1.*) from names n1 group by least(n1.*) having least(n1.*)!=NULL;
select least(n1.*),n1.* from names n1 group by 2 having least(n1.*)!=NULL;
select least(n1.*),n1.* from names n1 group by 1 having least(n1.*)!=NULL; -- Postgres error n1.id in select list should be in groupby
select least(n1.*) from names n1 group by least(n1.*) having least(n1.*)!=NULL order by least(n1.*);
select least(n1.*) from names n1 group by least(n1.*) order by least(n1.*);
select least(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id;
select least(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by least(n1.*); -- Postgres n1.* in select list should be in greoup by
select least(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by least(n2.*); -- Postgres n1.* in select list should be in greoup by
select least(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by least(n1.*,n2.*);
select least(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by least(n2.*,n1.*); -- Postgres errro select list n1.* must be in groupby
select least(n1.*,n2.*) from names n1 join names n2 on n1.id=n2.id group by 1 order by 1;
select 1 from names n1 join names n2 on n1.id=n2.id group by 1 order by least(n1.*,n2.*); -- Postgres error order by n1.* must appear in groupby
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.* order by least(n1.*,n2.*); -- Postgres error order by n2.* must appear in groupby
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.*,n2.* order by least(n1.*,n2.*);
select 1 from names n1 join names n2 on n1.id=n2.id group by n1.*,least(n1.*,n2.*),n2.* order by least(n1.*),least(n1.*,n2.*);



SELECT CASE n1.* WHEN NULL THEN lastname ELSE firstname||'test' end FROM names n1;
SELECT CASE n1.* WHEN NULL THEN lastname ELSE firstname||'test' end FROM names n1 group by n1.*,lastname,firstname;
SELECT CASE n1.* WHEN NULL THEN lastname ELSE firstname||'test' end FROM names n1 group by n1.*,lastname; -- Postgres error firstname not in groupby
SELECT CASE n1.* WHEN NULL THEN lastname ELSE firstname||'test' end FROM names n1 group by lastname,firstname; -- Postgres error n1.* not in groupby
SELECT CASE n1.* WHEN NULL THEN lastname ELSE firstname||'test' end FROM names n1 group by CASE n1.* WHEN NULL THEN lastname ELSE firstname||'test' end;
SELECT CASE n1.* WHEN NULL THEN lastname ELSE firstname||'test' end FROM names n1 group by CASE n1.* WHEN NULL THEN lastname end; -- Postgres error n1.* not in groupbyu
select case firstname when NULL then n1.* else firstname||'test' end from names n1 group by firstname,n1.*; -- Postgres error saying CASE types text and names cannot be matched
select case n1.* when n1.* then n1.* else firstname||'test' end from names n1 group by firstname,n1.*; -- Postgres error saying CASE types text and names cannot be matched
select case n1.* when NULL then firstname else n1.* end from names n1 group by firstname,n1.*; -- Postgres error saying CASE types text and names cannot be matched
select case n1.* when NULL then NULL else n1.* end from names n1 group by firstname,n1.*;
select case NULL when NULL then n1.* else n1.* end from names n1 group by n1.*;
select case n1.* when NULL then n1.* else n1.* end from names n1 group by n1.*;
select case n1.* when NULL then n1.* else n1.* end from names n1 group by case n1.* when NULL then n1.* else n1.* end;
select case n1.* when NULL then n1.* else n1.* end from names n1 group by case n1.* when NULL then n1.* else NULL end; -- Posgres error n1.* must appear in groupby
select case n1.* when NULL then n1.* else n1.* end from names n1 group by case n1.* when NULL then n1.* else NULL end,n1.*;
select case n1.* when NULL then n1.* else n1.* end from names n1 group by case n1.* when NULL then n1.* else n1.* end order by case n1.* when NULL then n1.* else n1.* end;
select case n1.* when NULL then n1.* else n1.* end from names n1 group by case n1.* when NULL then n1.* else n1.* end having n1.*=case n1.* when NULL then n1.* else n1.* end; -- Syntax error in having
select case n1.* when NULL then n1.* else n1.* end from names n1 group by case n1.* when NULL then n1.* else n1.* end having NULL=case n1.* when NULL then n1.* else n1.* end;
SELECT CASE n1.* WHEN NULL THEN lastname ELSE firstname||'test' end FROM names n1 group by 1;
select case firstname when NULL then n1.* else firstname||'test' end from names n1 group by 1; -- Postgres error saying CASE types text and names cannot be matched
select case n1.* when NULL then n1.* else n1.* end from names n1 group by 1;
select case n1.* when NULL then n1.* else n1.* end from names n1 group by case n1.* when NULL then n1.* else NULL end,1;
select case n1.* when NULL then n1.* else n1.* end from names n1 group by case n1.* when NULL then n1.* else n1.* end order by 1;
select case n1.* when NULL then n1.* else n1.* end from names n1 group by 1 having NULL=case n1.* when NULL then n1.* else n1.* end;

select NULL between n1.* AND NULL from names n1;
select NULL between n1.* AND NULL from names n1 group by NULL between n1.* AND NULL;
select NULL between n1.* AND NULL from names n1 group by NULL between n1.* AND NULL order by NULL between n1.* AND NULL;
select NULL between n1.* AND NULL from names n1 group by n1.*;
select NULL between n1.* AND NULL from names n1 group by NULL between NULL AND n1.*; -- Postgres error n1.* must be in groupby
select NULL between n1.* AND NULL from names n1 group by NULL between n1.* AND NULL having NULL between n1.* AND NULL;
select NULL between n1.* AND NULL from names n1;
select NULL between n1.* AND NULL from names n1 group by 1;
select NULL between n1.* AND NULL from names n1 group by 1 order by NULL between n1.* AND NULL;
select NULL between n1.* AND NULL from names n1 group by 1 order by 1;
select NULL between n1.* AND NULL from names n1 group by 1 having NULL between n1.* AND NULL;
select NULL between n1.* AND NULL from names n1 group by 1 having NULL between NULL AND n1.*; -- Postgres error n1.* must be in groupby



select NULL not between n1.* AND NULL from names n1;
select NULL not between n1.* AND NULL from names n1 group by NULL not between n1.* AND NULL;
select NULL not between n1.* AND NULL from names n1 group by NULL not between n1.* AND NULL order by NULL not between n1.* AND NULL;
select NULL not between n1.* AND NULL from names n1 group by n1.*;
select NULL not between n1.* AND NULL from names n1 group by NULL not between NULL AND n1.*; -- Postgres error n1.* must be in groupby
select NULL not between n1.* AND NULL from names n1 group by NULL not between n1.* AND NULL having NULL not between n1.* AND NULL;
select NULL not between n1.* AND NULL from names n1;
select NULL not between n1.* AND NULL from names n1 group by 1;
select NULL not between n1.* AND NULL from names n1 group by 1 order by NULL not between n1.* AND NULL;
select NULL not between n1.* AND NULL from names n1 group by 1 order by 1;
select NULL not between n1.* AND NULL from names n1 group by 1 having NULL not between n1.* AND NULL;
select NULL not between n1.* AND NULL from names n1 group by 1 having NULL not between NULL AND n1.*; -- Postgres error n1.* must be in groupby
select NULL not between n1.* AND NULL from names n1 group by 1 having NULL between n1.* AND NULL; -- Postgres error n1.* must be in groupby
