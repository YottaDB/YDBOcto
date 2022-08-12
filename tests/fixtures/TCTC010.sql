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

-- TCTC010 : OCTO582 : Test UNIQUE(id1, id2) and UNIQUE(id2, id1) constraint each go into a separate global name

create table tmp (id1 integer, id2 integer, UNIQUE(id1), UNIQUE(id2), UNIQUE(id1, id2), UNIQUE(id2, id1));
insert into tmp values (3, 4);

