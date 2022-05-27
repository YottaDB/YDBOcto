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

/* Helper SQL function to invoke arbitrary M code (useful for testing).
 * Note: This cannot be used in non-test code (e.g. octo-seed.sql) due to security issues (SQL injection attacks etc.).
 */
CREATE FUNCTION XECUTE_M_CODE(VARCHAR) RETURNS INTEGER AS $$XecuteMCode^xecutemcode

