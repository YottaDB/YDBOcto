#################################################################
#								#
# Copyright (c) 2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TC023 : OCTO320 : Octo treats empty column values as NULL when NOT NULL is not specified in the DDL

SELECT * FROM names;

-- Confirm correct use of NULLs in NATURAL JOIN between tables with heterogeneous columns
SELECT * FROM names NATURAL JOIN nullnames;

-- Select a combination of populated and NULL/empty fields
SELECT proname,pronargs,prorettype,proargtypes,proargmodes,protrftypes,probin FROM pg_proc;

