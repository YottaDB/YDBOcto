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
-- TVD0014 : YDBOcto#1108 : piece-of-piece over VistA Lab "CH" chemistry results
-- Each per-test result lives at ^LR(LRDFN,"CH",INVDT,TEST) = "<1>^<2>^<3>^...^<11>" (top level "^"-delimited).
-- Pieces 3 (workload) and 5 (reference ranges/units) are themselves "!"-sub-delimited, so the columns that
-- read them use DELIMS ("^","!") PIECES (<top>,<sub>) == $PIECE($PIECE(node,"^",<top>),"!",<sub>).
CREATE TABLE `LAB_CHEM_RESULT`(
 `LRDFN`  INTEGER PRIMARY KEY START 0 ENDPOINT '$CHAR(0)',
 `INVDT`  NUMERIC KEY NUM 1 START 0 ENDPOINT '$CHAR(0)',
 `TEST`   INTEGER KEY NUM 2 START 0 ENDPOINT '$CHAR(0)',
 `RESULT_VALUE`      VARCHAR PIECE 1,
 `RESULT_FLAG`       VARCHAR PIECE 2,
 `NATIONAL_LAB_CODE` VARCHAR DELIMS ("^","!") PIECES (3,1),
 `RESULT_NLT_CODE`   VARCHAR DELIMS ("^","!") PIECES (3,2),
 `LOINC_CODE`        VARCHAR DELIMS ("^","!") PIECES (3,3),
 `WORKLOAD_SUFFIX`   VARCHAR DELIMS ("^","!") PIECES (3,4),
 `TEST_IEN_FILE_60`  INTEGER DELIMS ("^","!") PIECES (3,7),
 `VERIFYING_TECH`    INTEGER PIECE 4,
 `SITE_SPECIMEN`     INTEGER DELIMS ("^","!") PIECES (5,1),
 `REFERENCE_LOW`     VARCHAR DELIMS ("^","!") PIECES (5,2),
 `REFERENCE_HIGH`    VARCHAR DELIMS ("^","!") PIECES (5,3),
 `CRITICAL_LOW`      VARCHAR DELIMS ("^","!") PIECES (5,4),
 `CRITICAL_HIGH`     VARCHAR DELIMS ("^","!") PIECES (5,5),
 `UNITS`             VARCHAR DELIMS ("^","!") PIECES (5,7),
 `DELTA_CHECK_TYPE`  VARCHAR DELIMS ("^","!") PIECES (5,8),
 `DELTA_VALUE`       VARCHAR DELIMS ("^","!") PIECES (5,9),
 `DEFAULT_VALUE`     VARCHAR DELIMS ("^","!") PIECES (5,10),
 `THERAPEUTIC_LOW`   VARCHAR DELIMS ("^","!") PIECES (5,11),
 `THERAPEUTIC_HIGH`  VARCHAR DELIMS ("^","!") PIECES (5,12),
 `INSTITUTION`       INTEGER PIECE 9,
 `EQUIPMENT_ID`      VARCHAR PIECE 11
)
GLOBAL "^LR(keys(""lrdfn""),""CH"",keys(""invdt""),keys(""test""))"
DELIM "^"
READONLY;

select * from LAB_CHEM_RESULT WHERE LRDFN=27 AND RESULT_FLAG='H*';
