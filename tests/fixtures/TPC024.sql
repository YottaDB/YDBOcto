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

-- TPC024 : OCTO597 : Test that SET/SHOW commands works for all supported variables

SET client_encoding = 'TEST';
SET datestyle = 'TEST';
SET integer_datetimes = 'TEST';
SET intervalstyle = 'TEST';
SET server_encoding = 'TEST';
SET server_version = 'TEST';
SET standard_confirming_strings = 'TEST';
SET timezone = 'TEST';

-- The following are read-only variables
SET is_superuser = 'TEST';
SET session_authorization = 'TEST';

SHOW client_encoding;
SHOW datestyle;
SHOW integer_datetimes;
SHOW intervalstyle;
SHOW server_encoding;
SHOW server_version;
SHOW standard_confirming_strings;
SHOW timezone;
SHOW is_superuser;
SHOW session_authorization;
