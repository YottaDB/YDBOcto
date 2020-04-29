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

-- TCF017 : OCTO345 : Single error output for nested erroneous function calls

CREATE FUNCTION LENGTH(VARCHAR) RETURNS INTEGER AS $$^LENGTH;

SELECT ABS(ABS(1, 2)) FROM names;
SELECT ABS(ABS(ABS(1, 2))) FROM names;

SELECT LENGTH(LENGTH(1, 2)) FROM names;
SELECT LENGTH(LENGTH(LENGTH(1, 2))) FROM names;

