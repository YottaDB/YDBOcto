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

-- TCT019 : OCTO636 : SIZE/PRECISION and/or SCALE differences in typecast operator should not result in different plans

SELECT 1.49::NUMERIC;
SELECT 1.49::NUMERIC(2);
SELECT 1.49::NUMERIC(1);
SELECT 1.49::NUMERIC(2,1);
SELECT 1.465::NUMERIC(3,2);

SELECT 2.49::NUMERIC;
SELECT 2.49::NUMERIC(2);
SELECT 2.49::NUMERIC(1);
SELECT 2.49::NUMERIC(2,1);
SELECT 2.465::NUMERIC(3,2);

