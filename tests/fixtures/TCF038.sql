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

-- TCF038 : OCTO929 : Test that repeated auto upgrade runs work fine with user defined functions

CREATE FUNCTION tmpFunc(integer) returns integer as $$^func;
CREATE TABLE tmpTable (id integer);
CREATE VIEW tmpView as select * from names;

