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

-- Define M function to hang. Helps simulate slow running queries if say used in a WHERE clause

CREATE FUNCTION HANGTIME(VARCHAR, INTEGER) RETURNS VARCHAR AS $$^HANGTIME;
CREATE FUNCTION HANGTIME(VARCHAR, NUMERIC) RETURNS VARCHAR AS $$^HANGTIME;

