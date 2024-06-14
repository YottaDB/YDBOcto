#################################################################
#								#
# Copyright (c) 2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

select replace('abca','a','b');
select replace(NULL,'a','b');
select replace('SQL asAdSQL asd SQL','SQL','LQS');
select replace('abca','a','ab');
select replace('aab','bc','to');
select replace('roger cat hello cat asdfasdfcatasdf','cat','dog');
select replace('aabcek','bca','to');
select replace('aabcek','bc','to');

select replace('ABCA','A','B');
select replace(NULL,'A','B');
select replace('sql ASaDsql ASD sql','sql','lqs');
select replace('roger cat hello cat asdfasdfcatasdf','cat',' ');
select replace('abca','c','');
select replace('ABCA','C','');
