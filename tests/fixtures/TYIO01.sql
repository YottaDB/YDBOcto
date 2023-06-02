#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TYIO01 : Test %YDBJNLF Octo functionality

-- A lot of the tests just do COUNT etc as the contents of the data contains
--    user ids, machine names, and process ids, which vary.

\d;
SELECT label, jnlfilename, dbfilename FROM YDBJNLF;
SELECT COUNT(*) FROM YDBJNLFACTIVE;
SELECT COUNT(*) FROM YDBJNLFCOMPLETE;
SELECT * FROM YDBJNLFOPCOUNT WHERE occurs > 0;
SELECT RECTYPE, TBLTYPE FROM YDBJNLFRECTYPE;
-- This is the most common query that users will perform; rest are sanity checks for other tables
SELECT op,gvname,gvref,nodeval FROM YDBJNLFTYPE1 WHERE gvname='^pastas';
SELECT op,gvname,gvref,nodeval FROM YDBJNLFTYPE1 WHERE gvref='^%ydboctoocto("oid")';
SELECT op FROM YDBJNLFTYPE10;
SELECT op FROM YDBJNLFTYPE4;
SELECT op FROM YDBJNLFTYPE5;
SELECT op FROM YDBJNLFTYPE6;
