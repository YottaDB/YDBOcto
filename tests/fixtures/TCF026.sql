#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF026 : OCTO595 : CREATE FUNCTION IF NOT EXISTS skips existing function of the same name with warning

CREATE FUNCTION IF NOT EXISTS PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;
SELECT PARMLESSFUNC() FROM names LIMIT 1;
CREATE FUNCTION IF NOT EXISTS PARMLESSFUNC() RETURNS VARCHAR AS $$^PARMLESSFUNC;

