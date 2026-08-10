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

-- TQO003 : #1141 : The schema name is optional inside OPERATOR()

select id from names where id operator(=) 3;
select id from names where id operator(<>) 3;
select id from names where id operator(!=) 3;
select id from names where id operator(<) 3;
select id from names where id operator(>) 3;
select id from names where id operator(<=) 3;
select id from names where id operator(>=) 3;
