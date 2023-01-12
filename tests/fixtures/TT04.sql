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

-- TT04 : OCTO609 : TRUNCATE on value expressions does not cause SIG-11 or misleading errors

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/609#note_1237730670
truncate 2+3;
truncate 2;
truncate false;

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/609#note_1237733914
truncate (names);
truncate (a);
truncate (((names)));
truncate (((a)));

