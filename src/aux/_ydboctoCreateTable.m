;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2026 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -------------------------------------------------------------
; Helper functions used by the CREATE TABLE command.
; -------------------------------------------------------------

%ydboctoCreateTable	;
	QUIT

validateGlobal(gvn)	;
	; Verify that the global variable name "gvn" (the portion of a table's GLOBAL keyword up to the
	; first subscript) can be accessed. "gvn" may be an extended reference such as ^["x.gld"]name or
	; ^|"x.gld"|name. $DATA with name indirection (@gvn) forces YottaDB to resolve any extended
	; reference in "gvn" exactly as the KILL @gvn done by DROP TABLE / TRUNCATE TABLE later would.
	; If the reference cannot be resolved (e.g. the user pointed it at a .dat instead of a .gld, or at
	; a file that does not exist), the $DATA errors; that error is deliberately NOT trapped here so it
	; propagates to the ydb_ci() caller (run_query.c), which rejects the CREATE TABLE. This catches
	; the bad GLOBAL at CREATE time instead of leaving behind a table that can be neither queried nor
	; dropped (YDBOcto#1122). A plain $DATA check is all that is needed.
	NEW x
	SET x=$DATA(@gvn)
	QUIT
