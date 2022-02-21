#################################################################
#								#
# Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TBT09 : OCTO498 : Confirm proper number of plans generated for queries using IS (NOT) TRUE, IS (NOT) FALSE, and IS (NOT) UNKNOWN" {

select id::boolean is false from names;
select id::boolean is true from names;
select id::boolean is null from names;
select id::boolean is unknown from names;
select id::boolean is not false from names;
select id::boolean is not true from names;
select id::boolean is not null from names;
select id::boolean is not unknown from names;
