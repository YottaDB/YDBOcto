-- Public Domain
-- Taken from https://www.nlm.nih.gov/research/umls/rxnorm/docs/techdoc.html and adapted for Octo
-- ##############################################################################
-- #										#
-- # Modified query Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	#
-- # All rights reserved.							#
-- #										#
-- #	This source code contains the intellectual property			#
-- #	of its copyright holder(s), and is made available			#
-- #	under a license.  If you do not know the terms of			#
-- #	the license, please stop and do not read further.			#
-- #										#
-- ##############################################################################
CREATE TABLE RXNREL (
  RXCUI1   VARCHAR(8),
  RXAUI1   VARCHAR(8),
  STYPE1   VARCHAR(50),
  REL      VARCHAR(4)  NOT NULL,
  RXCUI2   VARCHAR(8),
  RXAUI2   VARCHAR(8),
  STYPE2   VARCHAR(50),
  RELA     VARCHAR(100),
  RUI      VARCHAR(10),
  SRUI     VARCHAR(50),
  SAB      VARCHAR(20) NOT NULL,
  SL       VARCHAR(1000),
  DIR      VARCHAR(1),
  RG       VARCHAR(10),
  SUPPRESS VARCHAR(1),
  CVF      VARCHAR(50)
) GLOBAL "^RXNREL";
