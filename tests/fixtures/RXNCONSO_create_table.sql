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
CREATE TABLE RXNCONSO
(
  RXCUI    VARCHAR(8)                                    NOT NULL,
  LAT      VARCHAR(3)                                    NOT NULL,
  TS       VARCHAR(1),
  LUI      VARCHAR(8),
  STT      VARCHAR(3),
  SUI      VARCHAR(8),
  ISPREF   VARCHAR(1),
  RXAUI    VARCHAR(8)                                    NOT NULL,
  SAUI     VARCHAR(50),
  SCUI     VARCHAR(50),
  SDUI     VARCHAR(50),
  SAB      VARCHAR(20)                                   NOT NULL,
  TTY      VARCHAR(20)                                   NOT NULL,
  CODE     VARCHAR(50)                                   NOT NULL,
  STR      VARCHAR(3000)                                 NOT NULL,
  SRL      VARCHAR(10),
  SUPPRESS VARCHAR(1),
  CVF      VARCHAR(50)
) GLOBAL "^RXNCONSO";
