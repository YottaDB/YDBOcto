#################################################################
#								#
# Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################

-- TPC023 : OCTO597 : Test that SET/SHOW commands correctly update pg_settings

-- SET and SHOW using arbitrary case for variable names to validate case-insensitivity
SHOW DATESTYLE;
SET DaTeStYle = 'ISO';
SHOW dAtesTyle;

-- Confirm queries to pg_settings are case-sensitive.
-- The first query should produce a result row, while the latter should not.
select * from pg_settings where name = 'DateStyle';
select * from pg_settings where name = 'datestyle';

-- SHOW default empty/unset variable value
-- Use autovacuum_max_workers since Octo doesn't use/set this value by default.
-- If this value is ever set by default, this test should be updated to use another
-- known empty/unset runtime variable.
SHOW autovacuum_max_workers;

-- SET/SHOW default empty/unset variable value
SET datestyle = '';
SHOW datestyle;

-- Confirm pg_settings is updated by SET statements
select * from pg_settings where name = 'max_worker_processes';
SET max_worker_processes = '100';
select * from pg_settings where name = 'max_worker_processes';
