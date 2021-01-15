;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; user defined routine that can be called from octo
; just return what is passed in
HANGTIME(name,wait) ;
	; If the env var "mupip_intrpt" is set (currently only set by the TCR01 subtest), then send a MUPIP INTRPT to self.
	; Do it only once (as this function can be invoked multiple times for one query).
	; Do it before the hang to avoid any test timing issues.
	if (+$ztrnlnm("mupip_intrpt"))&(1=$increment(^intrptcntr)) zsystem "$ydb_dist/mupip intrpt "_$job
	hang wait
	quit name_wait
