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

-- TDRC03 : OCTO582 : Verify Global name in \d output with composite and vista-mini datasets

-- Below tables have multiple key columns (i.e. composite keys) and are mapped to globals as not just the
-- first few subscripts (i.e. they have some constant subscripts in between). This is a test to ensure the
-- constants also get printed fine in between.

\d composite;
\d ORDER_ORDER_ACTIONS;

