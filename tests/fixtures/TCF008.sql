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

-- TCF008 : OCTO345 : M label of extrinsic function can have all digits

CREATE FUNCTION TESTDIGITS() RETURNS VARCHAR AS $$12345^TESTDIGITS;
select TESTDIGITS() from names;

