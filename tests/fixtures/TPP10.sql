#################################################################
#								#
# Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TPP10 : OCTO867 : Verify [= NULL] and [= ''] in WHERE clause generate same physical plan with key fixing optimization

select * from names where lastname = null;
select * from names where lastname = '';

