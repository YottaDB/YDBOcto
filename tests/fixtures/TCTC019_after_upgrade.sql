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

-- TCTC019 : CHECK constraint CASE with multiple WHEN branches survives an auto upgrade of its text definition

-- Below rows test the CHECK constraints after the auto upgrade. Only rows 1 and 3 of each table are accepted.
INSERT INTO TCTC019a VALUES (1, 'Zero', true);
INSERT INTO TCTC019a VALUES (2, 'One', true);
INSERT INTO TCTC019a VALUES (3, 'Two', false);
SELECT * FROM TCTC019a;
INSERT INTO TCTC019b VALUES (1, 1);
INSERT INTO TCTC019b VALUES (2, 2);
INSERT INTO TCTC019b VALUES (3, 3);
SELECT * FROM TCTC019b;
