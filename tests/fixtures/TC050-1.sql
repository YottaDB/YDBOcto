#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
# OCTO761 : DELIM "" with GLOBAL on a column crash with GVUNDEF error
CREATE TABLE delim_null (
        ID NUMERIC PRIMARY KEY START 0 ENDPOINT '" "',
        P01 CHARACTER(30) NOT NULL GLOBAL "^DELIMNULL(keys(""id""),0)" PIECE 1,
        P02 CHARACTER(30) GLOBAL "^DELIMNULL(keys(""id""),0)" PIECE 2,
        P11 CHARACTER(30) GLOBAL "^DELIMNULL(keys(""id""),1)" PIECE 1,
        P12 CHARACTER(30) GLOBAL "^DELIMNULL(keys(""id""),1)" PIECE 2,
        E2  CHARACTER(30) GLOBAL "^DELIMNULL(keys(""id""),2)" DELIM ""
) GLOBAL "^DELIMNULL(keys(""id""))";
select * from delim_null;
