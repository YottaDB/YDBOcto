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

-- Uncommented query below validates that an ambiguity error is not thrown when select list alias name matches FROM list table column.
-- The commented query (included in the error fixture in this subtest) demonstrates that an ambiguity error is thrown when select list alias has two column names matching the alias despite the second name not being a user specified alias.
-- select 'Zero' != 'Zero' as firstname, firstname from names order by firstname;
select 'Zero' != 'Zero' as firstname from names order by firstname;
