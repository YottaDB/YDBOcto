#################################################################
#								#
# Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TERR035 : OCTO288 : Test error raised for non-existent extrinsic function

CREATE FUNCTION BADEXTRINSIC() RETURNS VARCHAR AS $$^%ydboctobadextrinsic;
SELECT BADEXTRINSIC();