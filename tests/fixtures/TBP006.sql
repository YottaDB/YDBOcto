#################################################################
#								#
# Copyright (c) 2023-2024 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TBP006 : OCTO935 : Test no SIG-11 and/or incorrect ERR_UNKNOWN_TABLE_OR_VIEW error on an invalid SQL command

-- Queries that start with an invalid table name ("tablename" below) used to previously issue a ERR_UNKNOWN_TABLE_OR_VIEW error
-- They are now expected to issue a "syntax error" highlighting the first token (i.e. invalid command).
tablename;
tablename aliasname;

-- Queries that start with a valid table name ("names" below) used to previously issue an "unexpected SEMICOLON" error.
-- They are now expected to issue a "syntax error" highlighting the first token (i.e. invalid command).
names;
names aliasname;


-- The below queries used to previously SIG-11.
-- They are now expected to issue a "syntax error" highlighting the first token (i.e. invalid command).
IDENTIFIER_ALONE;
IDENTIFIER_BACK_TICK;
IDENTIFIER_PERIOD_IDENTIFIER;

