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

select n1.* in (NULL,NULL) from names n1;
select n1.* in (NULL,NULL) from names n1 group by n1.* in (NULL,NULL);
select n1.* in (NULL,NULL) from names n1 group by n1.* in (NULL,NULL) having n1.* in (NULL,NULL);
select n1.* in (NULL,NULL) from names n1 group by n1.* in (NULL,NULL) order by n1.* in (NULL,NULL);
select n1.* in (NULL,NULL) from names n1 group by n1.* order by n1.* in (NULL,NULL);
select n1.* in (NULL,NULL) from names n1 group by n1.* in (NULL,NULL) order by 1;
select n1.* in (NULL,NULL) from names n1 group by 1;
select n1.* in (NULL,NULL) from names n1 group by 1 having n1.* in (NULL,NULL);
select n1.* in (NULL,NULL) from names n1 group by 1 order by n1.* in (NULL,NULL);
select n1.* in (NULL,NULL) from names n1 group by 1 order by 1;
select NULL in (n1.*) from names n1;
select NULL in (n1.*) from names n1 group by NULL in (n1.*);
select NULL in (n1.*) from names n1 group by n1.*;
select NULL in (n1.*) from names n1 group by NULL in (n1.*) order by NULL in (n1.*);
select NULL in (n1.*) from names n1 group by 1;
select NULL in (n1.*) from names n1 group by 1 order by NULL in (n1.*);
select NULL in (n1.*) from names n1 group by 1 order by 1;
select NULL in (n1.*,NULL) from names n1;
select NULL in (NULL,n1.*) from names n1;
select n1.* in (NULL) from names n1;
select NULL in (n1.*) from names n1;
select n1.* in (n2.*) from (select 'test' union select 'num') n1, (select 'nottest' union select 'num') n2;
select n1.* in (n1.*) from (select 'test' union select 'num') n1;
select n1.* in (n2.*,n2.*) from (VALUES(NULL), (1)) n1, (VALUES(NULL), (1)) n2;
select n1.* in (n2.*) from (VALUES(NULL), (1)) n1, (VALUES(NULL), (1)) n2;
select n1.* in (n1.*) from (VALUES(NULL), (1)) n1;
select n1.* in (n1.*) from (VALUES(NULL), (NULL)) n1;
select n1.* in (n1.*) from (VALUES(NULL)) n1;
select n1.* in (NULL) from (VALUES(NULL)) n1;
select n1.* in (n2.*) from (select 'test' union select 'num' union select 'numtest') n1, (select 'nottest' union select 'num') n2;
select n1.* in (n1.*,n2.*) from (select 'test' union select 'num' union select 'numtest') n1, (select 'nottest' union select 'num') n2;
select n1.* in (n2.*) from (select 'test' union select 'num' union select 'numtest') n1, (select 'nottest' union select 'num') n2;
select n1.* in (n2.*) from (select 'test' union select NULL union select 'numtest') n1, (select 'nottest' union select 'num') n2;
select n1.* in (n2.*) from (select 'test' union select NULL union select 'numtest') n1, (select 'nottest' union select NULL) n2 order by n1.*;
select n1.* in (n2.*) from (select lastname from names)n1, (select lastname from names)n2;


select n1.* not in (NULL,NULL) from names n1;
select NULL not in (n1.*,NULL) from names n1;
select n1.* not in (NULL,n1.*) from names n1;
select NULL not in (NULL,n1.*,NULL) from names n1;
select n1.* not in (NULL,n1.*,NULL) from names n1;
select n1.* not in (NULL,NULL) from names n1;
select NULL not in (n1.*,NULL) from names n1;
select n1.* not in (NULL,n1.*) from names n1;
select NULL not in (NULL,n1.*,NULL) from names n1;
select n1.* not in (NULL,n1.*,NULL) from names n1;
select n1.* not in (NULL) from names n1;
select NULL not in (n1.*) from names n1;
select n1.* not in (n2.*) from (select 'test' union select 'num') n1, (select 'nottest' union select 'num') n2;
select n1.* not in (n1.*) from (select 'test' union select 'num') n1;
select n1.* not in (n2.*,n2.*) from (VALUES(NULL), (1)) n1, (VALUES(NULL), (1)) n2;
select n1.* not in (n2.*) from (VALUES(NULL), (1)) n1, (VALUES(NULL), (1)) n2;
select n1.* not in (n1.*) from (VALUES(NULL), (1)) n1;
select n1.* not in (n1.*) from (VALUES(NULL), (NULL)) n1;
select n1.* not in (n1.*) from (VALUES(NULL)) n1;
select n1.* not in (NULL) from (VALUES(NULL)) n1;
select n1.* not in (n2.*) from (select 'test' union select 'num' union select 'numtest') n1, (select 'nottest' union select 'num') n2;
select n1.* not in (n1.*,n2.*) from (select 'test' union select 'num' union select 'numtest') n1, (select 'nottest' union select 'num') n2;
select n1.* not in (n2.*) from (select 'test' union select 'num' union select 'numtest') n1, (select 'nottest' union select 'num') n2;
select n1.* not in (n2.*) from (select 'test' union select NULL union select 'numtest') n1, (select 'nottest' union select 'num') n2;
select n1.* not in (n2.*) from (select 'test' union select NULL union select 'numtest') n1, (select 'nottest' union select NULL) n2 order by n1.*;
select n1.* not in (n2.*) from (select lastname from names)n1, (select lastname from names)n2;
