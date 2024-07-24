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

-- TX11 : OCTO961 : Verify DISCARD XREFS table_name error cases
-- Verify DISCARD XREFS table_name fails with ERR_UNKNOWN_TABLE error if table_name is not known
DISCARD XREFS mynames;
-- Verify DISCARD XREFS table_name fails with ERR_IDENT_LENGTH error if table_name is too long
DISCARD XREFS abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijkl;

-- Command only works on tables, expect ERR_UNKNOWN_TABLE for the DISCARD below
CREATE VIEW v AS SELECT 1;
DISCARD XREFS v;

-- sql keyword specification in place of table name returns ERR_PARSE_FAILED
DISCARD XREFS unknown;
