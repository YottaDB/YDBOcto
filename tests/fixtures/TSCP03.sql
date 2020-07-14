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
-- TSCP03 : OCTO552 : Allow booleans anywhere that takes a `value_expression`
SELECT true = false;
SELECT true IN (true);
SELECT COALESCE(true, 1 = 0, NULL);
SELECT NULLIF(1 = 1, 0 = 1);
SELECT GREATEST(1 = 1, 0 = 1, 1 > 2);
SELECT LEAST(1 = 1, 0 = 1, 1 > 2);
SELECT DISTINCT nullnames.id, nullnames.exempt
	FROM nullnames CROSS JOIN nullnames AS alias1
	WHERE (NOT ('Acid' > 'Lord') OR (NOT (nullnames.salary < 25000.01)) OR (NOT (false < false)))
	ORDER BY nullnames.exempt, nullnames.id;
select 1 + (true)::integer;
select true::integer;
select (true)::integer;
select 1 = true::integer;
select true IS NULL;
select true IS NOT NULL;
-- make sure this parses
select 1 + 2::int;
select (1 + 2)::int;
-- make sure it has the correct precedence
-- NOTE: Octo truncates when casting to int, but postgres rounds,
-- so the example is a little contrieved.
select 1.7 + 2.4::int; -- 3.7
select (1.7 + 2.4)::int; -- 4.1
-- These tests required changes in lp_verify_structure.
select coalesce(true and true);
select coalesce(true or false);
select coalesce(not true);
select coalesce(1 in (1));
select coalesce(1 not in (1));
select coalesce(1 != 1);
select coalesce(1 < 1);
select coalesce(1 > 1);
select coalesce(1 <= 1);
select coalesce(1 >= 1);
select coalesce(exists(SELECT 1));
select coalesce(not exists(SELECT 1));
select coalesce(1 is null);
select coalesce(1 is not null);
-- Allow boolean operators in BETWEEN
select * from nullnames where (id = 2) between 1 = 1 and false;
-- Make sure that BETWEEN is unambiguous
SELECT true BETWEEN (true BETWEEN true AND false) AND false;
