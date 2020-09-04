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

-- TJC013 : OCTO435 : Identifiers accepted in SET statements

SET DateStyle TO PostgreSQL;
SHOW DateStyle;

SET DateStyle=ISO;
SHOW DateStyle;

SET DateStyle TO 'PostgreSQL';
SHOW DateStyle;

SET DateStyle='ISO';
SHOW DateStyle;

