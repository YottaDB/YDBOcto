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
-- TVD0015 : YDBOcto#763/#764 : SUBSTR over the VistA XULM LOCK DICTIONARY (file 8993)
--
-- LOCK_TEMPLATE is field .01 of file 8993: characters 1-245 of the ^XLM(8993,IEN,0) node.
-- The YDBOctoVistA mapper emits every "extract-start/extract-end" Fileman field this way.
-- Before YDBOctoVistA v1.13 (YDBOctoVistA!43) that was an un-indexable EXTRACT:
--     `LOCK_TEMPLATE` ... EXTRACT "$E($G(^XLM(8993,keys(""xulm_lock_dictionary_id""),0)),1,245)"
-- and is now a SUBSTR column naming its own GLOBAL:
--     `LOCK_TEMPLATE` ... SUBSTR 1-245 GLOBAL "^XLM(8993,keys(""xulm_lock_dictionary_id""),0)"
-- Same value, but Octo can now build an AIM cross reference on it, so the WHERE and the
-- MIN/MAX below are served from the index instead of a full scan of file 8993.

-- 1) Every lock template. ORDER BY makes the row order independent of IEN assignment.
select LOCK_TEMPLATE from `XULM_LOCK_DICTIONARY` order by LOCK_TEMPLATE;

-- 2) Equality on the SUBSTR column: this is what could not be indexed before #763/#764.
select XULM_LOCK_DICTIONARY_ID, LOCK_TEMPLATE from `XULM_LOCK_DICTIONARY`
	where LOCK_TEMPLATE = 'DPT(DFN)';

-- 3) MIN/MAX read straight off the AIM index (YDBOcto#617).
select MIN(LOCK_TEMPLATE) as LO, MAX(LOCK_TEMPLATE) as HI from `XULM_LOCK_DICTIONARY`;

-- 4) A prefix match: the leading characters of a lock template are the VistA package
--    namespace, so this returns the templates belonging to the DG (Registration) package.
select LOCK_TEMPLATE from `XULM_LOCK_DICTIONARY`
	where LOCK_TEMPLATE like 'DG%' order by LOCK_TEMPLATE;

-- 5) The SUBSTR column alongside the ordinary PIECE columns of the same file, to confirm
--    a SUBSTR column and PIECE columns of one table coexist in a single plan.
select LOCK_TEMPLATE, GLOBAL_LOCK, PARTIAL_MATCH_ALLOWED from `XULM_LOCK_DICTIONARY`
	order by LOCK_TEMPLATE;

-- 6) NULL handling on a SUBSTR column: LOCK_TEMPLATE is the .01 field and is declared NOT NULL,
--    so the two counts below partition the file.
select count(*) as NULL_COUNT from `XULM_LOCK_DICTIONARY` where LOCK_TEMPLATE is null;
select count(*) as NOT_NULL_COUNT from `XULM_LOCK_DICTIONARY` where LOCK_TEMPLATE is not null;
