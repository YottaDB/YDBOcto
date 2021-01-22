#################################################################
#								#
# Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TCF005 : OCTO345 : M extrinsic functions created by CREATE FUNCTION are case sensitive

-- Map function to existing extrinsic function (all caps)
CREATE FUNCTION MODULO(INTEGER, INTEGER) RETURNS INTEGER AS $$^MODULO;
SELECT id, MODULO(id,2) FROM names;
DROP FUNCTION MODULO(INTEGER, INTEGER);

-- Map function to non-existing extrinsic function (lowercase)
CREATE FUNCTION MODULO(INTEGER, INTEGER) RETURNS INTEGER AS $$^modulo;
SELECT id, MODULO(id,2) FROM names;
DROP FUNCTION MODULO(INTEGER, INTEGER);

-- Map function to non-existing extrinsic function (mixed case)
CREATE FUNCTION MODULO(INTEGER, INTEGER) RETURNS INTEGER AS $$^mOdUlO;
SELECT id, MODULO(id,2) FROM names;
DROP FUNCTION MODULO(INTEGER, INTEGER);

