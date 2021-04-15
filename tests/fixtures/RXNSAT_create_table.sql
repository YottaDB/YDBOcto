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
CREATE TABLE RXNSAT
(
  RXCUI    VARCHAR(8)    NOT NULL,
  LUI      VARCHAR(8),
  SUI      VARCHAR(8),
  RXAUI    VARCHAR(8)    NOT NULL,
  STYPE    VARCHAR(50),
  CODE     VARCHAR(50),
  ATUI     VARCHAR(11),
  SATUI    VARCHAR(50),
  ATN      VARCHAR(1000) NOT NULL,
  SAB      VARCHAR(20)   NOT NULL,
  ATV      VARCHAR(4000) NOT NULL,
  SUPPRESS VARCHAR(1),
  CVF      VARCHAR(50)
) GLOBAL "^RXNSAT";
