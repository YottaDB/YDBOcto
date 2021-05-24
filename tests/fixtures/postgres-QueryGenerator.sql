-- ######################################################################
-- #									#
-- # Copyright (c) 2020-2021 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.						#
-- #									#
-- #	This source code contains the intellectual property		#
-- #	of its copyright holder(s), and is made available		#
-- #	under a license.  If you do not know the terms of		#
-- #	the license, please stop and do not read further.		#
-- #									#
-- ######################################################################

DROP FUNCTION IF EXISTS SAMEVALUE(integer);
CREATE FUNCTION SAMEVALUE(integer) RETURNS integer
    AS 'select $1;'
    LANGUAGE SQL
    IMMUTABLE;
DROP FUNCTION IF EXISTS SAMEVALUE(numeric);
CREATE FUNCTION SAMEVALUE(numeric) RETURNS numeric
    AS 'select $1;'
    LANGUAGE SQL
    IMMUTABLE;
DROP FUNCTION IF EXISTS SAMEVALUE(varchar);
CREATE FUNCTION SAMEVALUE(varchar) RETURNS varchar
    AS 'select $1;'
    LANGUAGE SQL
    IMMUTABLE;
DROP FUNCTION IF EXISTS SAMEVALUE(boolean);
CREATE FUNCTION SAMEVALUE(boolean) RETURNS boolean
    AS 'select $1;'
    LANGUAGE SQL
    IMMUTABLE;

