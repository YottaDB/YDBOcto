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

-- TX08 : OCTO904 : Correct results from using xref plans (_ydboctoX*.m) with same name but different contents

-- Below test is from https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/904#description
CREATE TABLE tmp (id INTEGER PRIMARY KEY, firstName VARCHAR, lastName VARCHAR);
INSERT INTO tmp VALUES (1, 'Zero', 'Cool');
SELECT * from tmp where firstName = 'Zero';
DROP TABLE tmp;
CREATE TABLE tmp (id INTEGER PRIMARY KEY, lastName VARCHAR, firstName VARCHAR);
INSERT INTO tmp VALUES (1, 'Cool', 'Zero');
SELECT * from tmp where firstName = 'Zero';
DROP TABLE tmp;

