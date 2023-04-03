#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

drop view if exists TCV031_1v;
create view TCV031_1v as select * from names;
select (select id from TCV031_1v limit 1), (select firstname from TCV031_1v limit 1);
