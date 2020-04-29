#################################################################
#								#
# Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCT008 : test 2 coerced functions generate different M files
CREATE FUNCTION DOLLARZWRITE(INTEGER) RETURNS VARCHAR AS $ZWRITE;
CREATE FUNCTION DOLLARREV(INTEGER) RETURNS VARCHAR AS $REVERSE;

select DOLLARZWRITE(id)::integer from names limit 1;
select DOLLARREV(id)::integer from names limit 1;

