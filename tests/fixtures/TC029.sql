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

-- TC029 : OCTO320 : Select a combination of populated and NULL/empty fields

SELECT proname,pronargs,prorettype,proargtypes,proargmodes,protrftypes,probin FROM pg_proc;
