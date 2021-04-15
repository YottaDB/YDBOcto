-- Public Domain
-- Taken from https://www.nlm.nih.gov/research/umls/rxnorm/sourcereleasedocs/rxnorm.html
-- TSPEED008 - Speed Test RxNorm Sample 3
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

-- Currently (2021/04/21), the below query does not finish running in Octo
-- because Octo does not know yet how to re-order tables based on the number of
-- records in each table (rs has 3 million records; but rc has 225000 records)
-- See https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/317#note_556459826
-- SELECT distinct rs.atv as ndc, rs.rxcui, rc.tty as rxnorm_tty, rc.str as rxnorm_str, rs.suppress
-- FROM rxnsat rs, rxnconso rc
-- WHERE rs.atn = 'NDC'
--      AND rs.sab = 'RXNORM'
--      AND rs.rxcui = rc.rxcui
--      AND rc.sab = 'RXNORM'
--      AND rc.tty in ('SCD','SBD','GPCK','BPCK')
--      AND rc.rxcui = '310965'
--      ORDER BY rs.suppress;

-- Query re-written below:

SELECT distinct rs.atv as ndc, rs.rxcui, rc.tty as rxnorm_tty, rc.str as rxnorm_str, rs.suppress
FROM rxnconso rc, rxnsat rs
WHERE rc.rxcui = '310965'
     AND rs.rxcui = rc.rxcui
     AND rs.atn = 'NDC'
     AND rs.sab = 'RXNORM'
     AND rc.sab = 'RXNORM'
     AND rc.tty in ('SCD','SBD','GPCK','BPCK')
     ORDER BY rs.suppress;
