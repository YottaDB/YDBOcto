#################################################################
#								#
# Copyright (c) 2022-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
-- TCTC012 : OCTO582 : Various User Level UNIQUE Tests
-- C: Table UNIQUE Named constraint on a single NULLABLE field
DROP TABLE IF EXISTS objecttypes;
CREATE TABLE objecttypes (
	  id INT NOT NULL,
	  objecttype VARCHAR(255),
	  CONSTRAINT idx_objecttypes_objecttype UNIQUE (objecttype)
);
\d objecttypes;
SELECT XECUTE_M_CODE('zwrite ^%ydboctoUIJe0YA67EEcnjNUb2OoV1D');
INSERT INTO objecttypes values (1, 'aaa');
SELECT * FROM objecttypes;
SELECT XECUTE_M_CODE('zwrite ^%ydboctoUIJe0YA67EEcnjNUb2OoV1D');
-- This will fail
INSERT INTO objecttypes values (2, 'bbb'), (3, 'aaa');
SELECT * FROM objecttypes;
-- This will succeed
INSERT INTO objecttypes values (2, 'bbb');
SELECT * FROM objecttypes;
SELECT XECUTE_M_CODE('zwrite ^%ydboctoUIJe0YA67EEcnjNUb2OoV1D');
-- This will fail
UPDATE objecttypes SET objecttype = 'aaa' WHERE id = 2;
SELECT * FROM objecttypes;
DELETE FROM objecttypes where id = 2;
SELECT * FROM objecttypes;
SELECT XECUTE_M_CODE('zwrite ^%ydboctoUIJe0YA67EEcnjNUb2OoV1D');
DELETE FROM objecttypes where id = 1;
SELECT * FROM objecttypes;
SELECT XECUTE_M_CODE('zwrite ^%ydboctoUIJe0YA67EEcnjNUb2OoV1D');
DROP TABLE IF EXISTS objecttypes;
