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

-- TCTC008D : Test that multiple NOT NULL constraints can be specified
DROP TABLE IF EXISTS tmp;
CREATE TABLE tmp (id INTEGER CONSTRAINT notnull1 NOT NULL NOT NULL CONSTRAINT notnull3 not null);
\d tmp;
select * from tmp;
