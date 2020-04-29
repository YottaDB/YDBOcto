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

-- TCF018 : OCTO345 : Function return type correctly evaluated in boolean expressions

CREATE FUNCTION SAMESTR(VARCHAR) RETURNS VARCHAR AS $$^samestr;

SELECT * FROM names WHERE firstname > lastname;
SELECT * FROM names WHERE SAMESTR(firstname) > SAMESTR(lastname);

