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

-- TPC025 : OCTO597 : Test pg_settings xrefs are updated when value changed via SET command

-- Retrieve row for initial (default) value of a setting, i.e. DateStyle, to create a cross reference
select name, setting from pg_settings where setting = 'ISO, MDY';

-- Change the value to invalidate previously created cross reference
set datestyle = 'ISO';

-- Run the previous to confirm cross reference was invalidated, i.e. no results returned
select name, setting from pg_settings where setting = 'ISO, MDY';

-- Run query for the new value to generate new cross reference and confirm correct value
select name, setting from pg_settings where setting = 'ISO';

