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

select replace('abca','c','');
select replace('abca','','b');
select replace('ABCA','C','');
select replace('ABCA','','B');
select replace('','ABCB','B');
-- Octo in the following case will treat NULL input as '' but Postgres treats them as NULL itself
select replace('abca','c',NULL);
select replace('ABCA','C',NULL);
select replace('ABCA',NULL,'B');
select replace('abca',NULL,'b');
select replace('ABCA',NULL,'B');
