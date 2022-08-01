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

-- TERR047 : OCTO519 : Various unknown column error scenarios

-- Test ERR_UNKNOWN_COLUMN instead of SIG-11 for former lexer tokens, similar to commit 1937f49c (#935)
select IDENTIFIER_PERIOD_IDENTIFIER;
select IDENTIFIER_BACKTICK;
select IDENTIFIER_ALONE;
select PARENLESS_FUNCTION;

-- Test backtick and double quoted identifiers
SELECT `abcd` FROM names;
SELECT "abcd" FROM names;
