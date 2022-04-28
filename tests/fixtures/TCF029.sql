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

-- TCF029 : OCTO288 : Correct handling of maximum extrinsic function name length

CREATE FUNCTION functoolong() RETURNS INTEGER AS $$^%ydboctoabcdefghijklmnopqrstuvw;
SELECT functoolong();
