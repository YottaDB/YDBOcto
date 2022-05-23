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

select least(n1.*) from names n1;
select least(n1.*,n1.id) from names n1;
select least(n1.*,NULL) from names n1;
select greatest(n1.*,n1.id) from names n1;
select greatest(n1.*,NULL) from names n1;
select greatest(n1.*) from names n1;
select coalesce(n1.*,n1.id) from names n1;
select coalesce(n1.*,NULL) from names n1;
select coalesce(n1.*) from names n1;
select nullif(n1.*,n1.*) from names n1;
select nullif(n1.*,NULL) from names n1;
select nullif(n1.*,n1.id) from names n1;
select nullif(NULL,n1.*) from names n1;
select 1 from names n1 order by coalesce(n1.*);
select 1 from names n1 order by coalesce(n1.id, n1.*);
select 1 from names n1 order by coalesce(NULL, n1.*);
select 1 from names n1 order by greatest(n1.*);
select 1 from names n1 order by greatest(n1.id, n1.*);
select 1 from names n1 order by greatest(NULL, n1.*);
select 1 from names n1 order by nullif(n1.id, n1.*);
select 1 from names n1 order by nullif(NULL, n1.*);
select 1 from names n1 order by nullif(n1.*, n1.*);
select 1 from names n1 where coalesce(n1.*)=NULL;
select 1 from names n1 where greatest(n1.*)=NULL;
select 1 from names n1 where least(n1.*)=NULL;
select 1 from names n1 where nullif(n1.*,NULL)=NULL;
select 1 from names n1 where nullif(NULL,n1.*)=NULL;
select 1 from names n1 where nullif(n1.*,n1.*)=NULL;
select 1 from names n1 group by n1.* having coalesce(n1.*)=NULL;
select 1 from names n1 group by n1.* having greatest(n1.*)=NULL;
select 1 from names n1 group by n1.* having least(n1.*)=NULL;
select 1 from names n1 group by n1.* having nullif(n1.*,NULL)=NULL;
select 1 from names n1 group by n1.* having nullif(n1.*,n1.*)=NULL;

select 1 from names n1 where case NULL when NULL then n1.* else n1.* end;
select 1 from names n1 order by case NULL when NULL then n1.* else n1.* end;
select 1 from names n1 order by case NULL when NULL then n1.id else n1.* end;
select case NULL when NULL then n1.id else n1.* end from names n1;
select case NULL when NULL then n1.* else n1.* end from names n1;
select case n1.*=NULL when n1.* then n1.* else n1.* end from names n1;
select case n1.*=NULL when n1.*=NULL then n1.* else n1.* end from names n1;

select +n1.* from names n1;
select -n1.* from names n1;
