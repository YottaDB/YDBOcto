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
-- TSCP02 : OCTO544 : Assertion failure and Errors when IN is used in SELECT column list
SELECT 1 IN (true) FROM names;
-- TODO: this should give a type error, but currently gives an parse error (#552)
SELECT true IN (1) FROM names;
