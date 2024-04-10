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

CREATE FUNCTION absf(INTEGER) RETURNS INTEGER AS $$ABS^%ydboctosqlfunctions;
CREATE FUNCTION replacef(VARCHAR, VARCHAR, VARCHAR) RETURNS VARCHAR AS $$REPLACE^%ydboctosqlfunctions;
