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

-- TPP08 : OCTO694 : Test that xref plan gets regenerated if a physical plan that relies on it gets regenerated

-- Generate plan2 which uses xref plan of same LASTNAME column that TPP08.sql used
SELECT lastname from names WHERE lastname = 'Nikon';

