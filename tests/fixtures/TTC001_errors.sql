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

-- TTC001 : OCTO1019 : Type cast literals before storing in INTEGER or NUMERIC column values

-- Below queries are those which are commented in TTC001.sql.
-- They cannot be run there due to cross check issues for queries that generate an error.

-- Test of ERR_DUPLICATE_KEY_VALUE error
insert into tblINT values (5.499, 7);
insert into tblINT values (5.5, 8);
insert into tblINT values (5.501, 9);

