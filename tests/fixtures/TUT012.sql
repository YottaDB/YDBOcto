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

-- TUT012 : OCTO579 : Correct results from UPDATE when WHERE clause uses IN with a list of values

-- Test of https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/579#note_1095739069

DROP TABLE IF EXISTS tmp;
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR);
INSERT INTO tmp VALUES (1,'Cereal');
INSERT INTO tmp VALUES (2,'Lord');
INSERT INTO tmp VALUES (3,'Joey');
UPDATE tmp SET firstName='Joey'  WHERE firstName IN ('Lord', 'Joey', 'Cereal');

