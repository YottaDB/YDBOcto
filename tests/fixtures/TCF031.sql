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

-- TCF031 : OCTO816 : Octo issues error for ambiguous function calls

CREATE FUNCTION samevalue(integer) RETURNS integer AS $$a1^TCF031;
SELECT samevalue(NULL); -- Call $$a1^TCF031: return 1
SELECT samevalue(1); -- Call $$a1^TCF031: return 1
SELECT samevalue(1.0); -- Error: Unknown function
SELECT samevalue('1'); -- Error: Unknown function
SELECT samevalue(true); -- Error: Unknown function
CREATE FUNCTION samevalue(numeric) RETURNS integer AS $$a2^TCF031;
SELECT samevalue(NULL); -- Error: Function not unique
SELECT samevalue(1); -- Call $$a1^TCF031: return 1
SELECT samevalue(1.0); -- Call $$a2^TCF031: return 2
SELECT samevalue('1'); -- Error: Unknown function
SELECT samevalue(true); -- Error: Unknown function
CREATE FUNCTION samevalue(varchar) RETURNS integer AS $$a3^TCF031;
SELECT samevalue(NULL); -- Error: Function not unique
SELECT samevalue(1); -- Call $$a1^TCF031: return 1
SELECT samevalue(1.0); -- Call $$a2^TCF031: return 2
SELECT samevalue('1'); -- Call $$a2^TCF031: return 3
SELECT samevalue(true); -- Error: Unknown function
CREATE FUNCTION samevalue(boolean) RETURNS integer AS $$a4^TCF031;
SELECT samevalue(NULL); -- Error: Function not unique
SELECT samevalue(1); -- Call $$a1^TCF031: return 1
SELECT samevalue(1.0); -- Call $$a2^TCF031: return 2
SELECT samevalue('1'); -- Call $$a2^TCF031: return 3
SELECT samevalue(true); -- Call $$a4^TCF031: return 4

CREATE FUNCTION secondvalue(integer, integer) RETURNS integer AS $$b1^TCF031;
SELECT secondvalue(NULL, NULL); -- Call $$b1^TCF031: return 1
SELECT secondvalue(1, NULL); -- Call $$b1^TCF031: return 1
SELECT secondvalue(NULL, 1); -- Call $$b1^TCF031: return 1
SELECT secondvalue(1, 1); -- Call $$b1^TCF031: return 1
SELECT secondvalue(1.0, 1); -- Error: Unknown function
CREATE FUNCTION secondvalue(integer, numeric) RETURNS integer AS $$b2^TCF031;
SELECT secondvalue(NULL, NULL); -- Error: Function not unique
SELECT secondvalue(1, NULL); --  Error: Function not unique
SELECT secondvalue(1.0, 1); -- Error: Unknown function
SELECT secondvalue(1, 1); -- Call $$b1^TCF031: return 1
SELECT secondvalue(1, 1.0); -- Call $$b2^TCF031: return 2
CREATE FUNCTION secondvalue(integer, varchar) RETURNS integer AS $$b3^TCF031;
SELECT secondvalue(NULL, NULL); -- Error: Function not unique
SELECT secondvalue(1, NULL); --  Error: Function not unique
SELECT secondvalue('1', 1); -- Error: Unknown function
SELECT secondvalue(1, 1); -- Call $$b1^TCF031: return 1
SELECT secondvalue(1, 1.0); -- Call $$b2^TCF031: return 2
SELECT secondvalue(NULL, '1'); -- Call $$b3^TCF031: return 3
SELECT secondvalue(1, '1'); -- Call $$b3^TCF031: return 3
CREATE FUNCTION secondvalue(integer, boolean) RETURNS integer AS $$b4^TCF031;
SELECT secondvalue(NULL, NULL); -- Error: Function not unique
SELECT secondvalue(1, NULL); --  Error: Function not unique
SELECT secondvalue(true, 1); -- Error: Unknown function
SELECT secondvalue(1, 1); -- Call $$b1^TCF031: return 1
SELECT secondvalue(1, 1.0); -- Call $$b2^TCF031: return 2
SELECT secondvalue(1, '1'); -- Call $$b3^TCF031: return 3
SELECT secondvalue(NULL, true); -- Call $$b4^TCF031: return 4
SELECT secondvalue(1, true); -- Call $$b4^TCF031: return 4
CREATE FUNCTION secondvalue(varchar, integer) RETURNS integer AS $$b5^TCF031;
SELECT secondvalue(NULL, NULL); -- Error: Function not unique
SELECT secondvalue(NULL, 1); --  Error: Function not unique
SELECT secondvalue(1, 1); -- Call $$b1^TCF031: return 1
SELECT secondvalue(1, 1.0); -- Call $$b2^TCF031: return 2
SELECT secondvalue(1, '1'); -- Call $$b3^TCF031: return 3
SELECT secondvalue(NULL, true); -- Call $$b4^TCF031: return 4
SELECT secondvalue(1, true); -- Call $$b4^TCF031: return 4
SELECT secondvalue('1', NULL); -- Call $$b5^TCF031: return 5
SELECT secondvalue('1', 1); -- Call $$b5^TCF031: return 5
CREATE FUNCTION secondvalue(varchar, varchar) RETURNS integer AS $$b6^TCF031;
SELECT secondvalue(NULL, NULL); -- Error: Function not unique
SELECT secondvalue(NULL, '1'); --  Error: Function not unique
SELECT secondvalue('1', NULL); --  Error: Function not unique
SELECT secondvalue(1, 1); -- Call $$b1^TCF031: return 1
SELECT secondvalue(1, 1.0); -- Call $$b2^TCF031: return 2
SELECT secondvalue(1, '1'); -- Call $$b3^TCF031: return 3
SELECT secondvalue(NULL, true); -- Call $$b4^TCF031: return 4
SELECT secondvalue(1, true); -- Call $$b4^TCF031: return 4
SELECT secondvalue('1', 1); -- Call $$b5^TCF031: return 5
SELECT secondvalue('1', '1'); -- Call $$b6^TCF031: return 6

CREATE FUNCTION thirdvalue(integer, boolean, integer) RETURNS integer AS $$c1^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(1, NULL, NULL); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, NULL); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, NULL, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(1, NULL, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(1, true, NULL); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(1, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, 1, NULL); -- Error: Unknown function
CREATE FUNCTION thirdvalue(integer, boolean, numeric) RETURNS integer AS $$c2^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, true, NULL); -- Error: Function not unique
SELECT thirdvalue(1, true, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(1, NULL, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(1, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, NULL, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, true, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(1, NULL, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(1, true, 1.0); -- Call $$c2^TCF031: return 2
CREATE FUNCTION thirdvalue(integer, numeric, varchar) RETURNS integer AS $$c3^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, true, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, NULL, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, NULL); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, NULL, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(1, 1.0, NULL); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(1, NULL, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(1, 1.0, '1'); -- Call $$c3^TCF031: return 3
CREATE FUNCTION thirdvalue(integer, boolean, varchar) RETURNS integer AS $$c4^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, true, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, '1'); -- Error: Function not unique
SELECT thirdvalue(1, NULL, '1'); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, NULL, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, NULL); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(1, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(1, true, '1'); -- Call $$c4^TCF031: return 4
CREATE FUNCTION thirdvalue(integer, boolean, boolean) RETURNS integer AS $$c5^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, true, NULL); -- Error: Function not unique
SELECT thirdvalue(1, true, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, NULL, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, NULL); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(NULL, true, true); -- Call $$c5^TCF031: return 5
SELECT thirdvalue(1, true, true); -- Call $$c5^TCF031: return 5
CREATE FUNCTION thirdvalue(integer, numeric, integer) RETURNS integer AS $$c6^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, 1.0, NULL); -- Error: Function not unique
SELECT thirdvalue(1, 1.0, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, 1); -- Error: Function not unique
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, NULL, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(NULL, true, true); -- Call $$c5^TCF031: return 5
SELECT thirdvalue(NULL, 1.0, 1); -- Call $$c6^TCF031: return 6
SELECT thirdvalue(1, 1.0, 1); -- Call $$c6^TCF031: return 6
CREATE FUNCTION thirdvalue(integer, numeric, numeric) RETURNS integer AS $$c7^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, 1.0, NULL); -- Error: Function not unique
SELECT thirdvalue(1, 1.0, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, 1.0); -- Error: Function not unique
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(NULL, true, true); -- Call $$c5^TCF031: return 5
SELECT thirdvalue(NULL, 1.0, 1); -- Call $$c6^TCF031: return 6
SELECT thirdvalue(NULL, 1.0, 1.0); -- Call $$c7^TCF031: return 7
SELECT thirdvalue(1, 1.0, 1.0); -- Call $$c7^TCF031: return 7
CREATE FUNCTION thirdvalue(integer, varchar, numeric) RETURNS integer AS $$c8^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, 1.0); -- Error: Function not unique
SELECT thirdvalue(1, NULL, 1.0); -- Error: Function not unique
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(NULL, true, true); -- Call $$c5^TCF031: return 5
SELECT thirdvalue(NULL, 1.0, 1); -- Call $$c6^TCF031: return 6
SELECT thirdvalue(NULL, 1.0, 1.0); -- Call $$c7^TCF031: return 7
SELECT thirdvalue(NULL, '1', NULL); -- Call $$c8^TCF031: return 8
SELECT thirdvalue(1, '1', NULL); -- Call $$c8^TCF031: return 8
SELECT thirdvalue(NULL, '1', 1.0); -- Call $$c8^TCF031: return 8
SELECT thirdvalue(1, '1', 1.0); -- Call $$c8^TCF031: return 8
CREATE FUNCTION thirdvalue(integer, numeric, boolean) RETURNS integer AS $$c9^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, true); -- Error: Function not unique
SELECT thirdvalue(1, NULL, true); -- Error: Function not unique
SELECT thirdvalue(1, 1.0, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(NULL, true, true); -- Call $$c5^TCF031: return 5
SELECT thirdvalue(NULL, 1.0, 1); -- Call $$c6^TCF031: return 6
SELECT thirdvalue(NULL, 1.0, 1.0); -- Call $$c7^TCF031: return 7
SELECT thirdvalue(NULL, '1', NULL); -- Call $$c8^TCF031: return 8
SELECT thirdvalue(NULL, 1.0, true); -- Call $$c9^TCF031: return 9
SELECT thirdvalue(1, 1.0, true); -- Call $$c9^TCF031: return 9
CREATE FUNCTION thirdvalue(integer, varchar, varchar) RETURNS integer AS $$c10^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(1, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, '1'); -- Error: Function not unique
SELECT thirdvalue(1, NULL, '1'); -- Error: Function not unique
SELECT thirdvalue(1, '1', NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, '1', NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(NULL, true, true); -- Call $$c5^TCF031: return 5
SELECT thirdvalue(NULL, 1.0, 1); -- Call $$c6^TCF031: return 6
SELECT thirdvalue(NULL, 1.0, 1.0); -- Call $$c7^TCF031: return 7
SELECT thirdvalue(NULL, '1', 1.0); -- Call $$c8^TCF031: return 8
SELECT thirdvalue(NULL, 1.0, true); -- Call $$c9^TCF031: return 9
SELECT thirdvalue(NULL, '1', '1'); -- Call $$c10^TCF031: return 10
SELECT thirdvalue(1, '1', '1'); -- Call $$c10^TCF031: return 10
CREATE FUNCTION thirdvalue(numeric, varchar, varchar) RETURNS integer AS $$c11^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, '1'); -- Error: Function not unique
SELECT thirdvalue(NULL, '1', '1'); -- Error: Function not unique
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(NULL, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(NULL, true, true); -- Call $$c5^TCF031: return 5
SELECT thirdvalue(NULL, 1.0, 1); -- Call $$c6^TCF031: return 6
SELECT thirdvalue(NULL, 1.0, 1.0); -- Call $$c7^TCF031: return 7
SELECT thirdvalue(NULL, '1', 1.0); -- Call $$c8^TCF031: return 8
SELECT thirdvalue(NULL, 1.0, true); -- Call $$c9^TCF031: return 9
SELECT thirdvalue(1, '1', '1'); -- Call $$c10^TCF031: return 10
SELECT thirdvalue(1.0, NULL, NULL); -- Call $$c11^TCF031: return 11
SELECT thirdvalue(1.0, '1', NULL); -- Call $$c11^TCF031: return 11
SELECT thirdvalue(1.0, NULL, '1'); -- Call $$c11^TCF031: return 11
SELECT thirdvalue(1.0, '1', '1'); -- Call $$c11^TCF031: return 11
CREATE FUNCTION thirdvalue(varchar, boolean, varchar) RETURNS integer AS $$c12^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, true, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, NULL, '1'); -- Error: Function not unique
SELECT thirdvalue(NULL, true, '1'); -- Error: Function not unique
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(1, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(NULL, true, true); -- Call $$c5^TCF031: return 5
SELECT thirdvalue(NULL, 1.0, 1); -- Call $$c6^TCF031: return 6
SELECT thirdvalue(NULL, 1.0, 1.0); -- Call $$c7^TCF031: return 7
SELECT thirdvalue(NULL, '1', 1.0); -- Call $$c8^TCF031: return 8
SELECT thirdvalue(NULL, 1.0, true); -- Call $$c9^TCF031: return 9
SELECT thirdvalue(1, '1', '1'); -- Call $$c10^TCF031: return 10
SELECT thirdvalue(1.0, NULL, NULL); -- Call $$c11^TCF031: return 11
SELECT thirdvalue('1', NULL, NULL); -- Call $$c12^TCF031: return 12
SELECT thirdvalue('1', true, NULL); -- Call $$c12^TCF031: return 12
SELECT thirdvalue('1', NULL, '1'); -- Call $$c12^TCF031: return 12
SELECT thirdvalue('1', NULL, '1'); -- Call $$c12^TCF031: return 12
SELECT thirdvalue('1', true, '1'); -- Call $$c12^TCF031: return 12
CREATE FUNCTION thirdvalue(varchar, varchar, varchar) RETURNS integer AS $$c13^TCF031;
SELECT thirdvalue(NULL, NULL, NULL); -- Error: Function not unique
SELECT thirdvalue('1', NULL, NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, '1', NULL); -- Error: Function not unique
SELECT thirdvalue(NULL, '1', '1'); -- Error: Function not unique
SELECT thirdvalue('1', NULL, '1'); -- Error: Function not unique
SELECT thirdvalue(NULL, true, 1); -- Call $$c1^TCF031: return 1
SELECT thirdvalue(NULL, true, 1.0); -- Call $$c2^TCF031: return 2
SELECT thirdvalue(NULL, 1.0, '1'); -- Call $$c3^TCF031: return 3
SELECT thirdvalue(1, true, '1'); -- Call $$c4^TCF031: return 4
SELECT thirdvalue(NULL, true, true); -- Call $$c5^TCF031: return 5
SELECT thirdvalue(NULL, 1.0, 1); -- Call $$c6^TCF031: return 6
SELECT thirdvalue(NULL, 1.0, 1.0); -- Call $$c7^TCF031: return 7
SELECT thirdvalue(NULL, '1', 1.0); -- Call $$c8^TCF031: return 8
SELECT thirdvalue(NULL, 1.0, true); -- Call $$c9^TCF031: return 9
SELECT thirdvalue(1, '1', '1'); -- Call $$c10^TCF031: return 10
SELECT thirdvalue(1.0, NULL, NULL); -- Call $$c11^TCF031: return 11
SELECT thirdvalue('1', true, NULL); -- Call $$c12^TCF031: return 12
SELECT thirdvalue('1', '1', '1'); -- Call $$c13^TCF031: return 13
