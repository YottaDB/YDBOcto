#################################################################
#								#
# Copyright (c) 2021-2023 YottaDB LLC and/or its subsidiaries.	#
# All rights reserved.						#
#								#
#	This source code contains the intellectual property	#
#	of its copyright holder(s), and is made available	#
#	under a license.  If you do not know the terms of	#
#	the license, please stop and do not read further.	#
#								#
#################################################################
CREATE TABLE `RXNORM_CONC_NAMES_AND_SOUR`(
 `RXNORM_CONC_NAMES_AND_SOUR_ID` NUMERIC PRIMARY KEY START 0 ENDPOINT '$CHAR(0)',
 `RXCUI` CHARACTER(8) NOT NULL GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""),0)" PIECE 1,
 `SOURCE_SAB` CHARACTER(20) GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""),0)" PIECE 2,
 `TERM_TYPE_TTY` CHARACTER(20) GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""),0)" PIECE 3,
 `CODE` CHARACTER(50) GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""),0)" PIECE 4,
 `SUPPRESSION_FLAG_SUPPRESS` CHARACTER(1) GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""),0)" PIECE 5,
 `CONTENT_VIEW_FLAG_CVF` CHARACTER(50) GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""),0)" PIECE 6,
 `RXAUI` CHARACTER(8) GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""),0)" PIECE 7,
 `TEXT_STR` CHARACTER(3000) GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""),1)" PIECE 1,
 `ACTIVATION_DATE` NUMERIC GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""),""VA"")" PIECE 1
)
GLOBAL "^ETSRXN(129.2,keys(""rxnorm_conc_names_and_sour_id""))"
DELIM "^"
READONLY;
