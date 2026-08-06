#################################################################
#								#
# Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TMF03 : OCTO1030 : Test of MOD() function (matches the "%" operator's SQL standard behavior)

select mod(11,4);
select mod(11,-4);
select mod(-11,4);
select mod(-11,-4);
select mod(0,4);
select mod(0,-4);
select mod(8,-4);
select mod(-8,4);
select mod(11.5,-4);
select mod(11,-4.5);
select mod(11.5,-4.5);
select mod(NULL,-4);
select mod(11,NULL);
