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

-- TERR036 : OCTO288 : Test of ERR_UNKNOWN_FUNCTION_EMULATION/UNKNOWNFUNCTION error

-- Currently only tested with LPAD, as this is the only function that requires an M-level
-- error to be issued. However, other functions are likely to issue similar errors in the future.
select lpad("dfdf", 5);
