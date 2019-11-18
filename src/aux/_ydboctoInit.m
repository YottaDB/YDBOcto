;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -------------------------------------------------------------
; Below M code is invoked by Octo/Rocto at process startup
; It initializes a few error codes that are needed by the generated M plan `%ydboctoerror(...)`
; And does checks for null subscripts and returns 0 if checks succeed and 1 otherwise.
; Caller (Octo/Rocto) issue appropriate error on a non-zero return.
; -------------------------------------------------------------
%ydboctoInit()	;
	; -----------------------------------------------------------
	; Perform error code initialization for generated M plan
	; The M code (invoked through "ydb_ci") could detect an error situation that is not a YDB error.
	; In that case, we want to pass that error code back to "run_query" (in Octo/Rocto).
	; The way we do that is to do a ZMESSAGE in the M code of an error code number that is guaranteed to
	; not be a valid YDB error code and have "run_query" check for these error codes before treating it
	; as a YDB error. Use an impossible error code (> 2*30) for these Octo-level error codes.
	; -----------------------------------------------------------
	NEW %ydboctoerrcode
	SET %ydboctoerrcodemin=(2**30)
	SET %ydboctoerrcode=%ydboctoerrcodemin
	; Below is the list of possible Octo internal errors (relied upon by `src/ydb_error_check.c`).
	SET %ydboctoerror("SUBQUERYMULTIPLEROWS")=$incr(%ydboctoerrcode) ; signaled by `GetScalar` in `_ydboctoplanhelpers.m`
	; Any additions of error codes needs to happen before the following line (%ydboctoerrcodemax)
	; Changes need to also happen in `ydb_error_check.c` and likely in `_ydboctoplanhelpers.m`
	SET %ydboctoerrcodemax=$incr(%ydboctoerrcode)
	; Set $ETRAP for Octo. Note that "ydb_env_set" sets this ISV to a default value that does a WRITE $ZSTATUS.
	; This can be user-unfriendly for Octo since we would see $ZSTATUS show up for non-YDB errors (which Octo simulates above).
	; Hence the need to clear $ETRAP at Octo startup.
	SET $ETRAP=""
	; -----------------------------------------------------------
	; Perform NullSubs check
	; -----------------------------------------------------------
	NEW verified,octogbl,reglist,regnum,regname,numregs
	SET quit=0
	FOR octogbl="^%ydboctoxref","^%ydboctoocto","^%ydboctoschema" DO
	. SET reglist=$VIEW("REGION",octogbl),numregs=$LENGTH(reglist,",")
	. FOR regnum=1:1:numregs DO
	. . SET regname=$PIECE(reglist,",",regnum)
	. . QUIT:$DATA(verified(regname))
	. . IF ($$^%PEEKBYNAME("sgmnt_data.null_subs",regname)'=1) DO
	. . . WRITE "ERROR: Null subscripts must be enabled for global "_octogbl_" in region "_regname,!
	. . . SET quit=1
	. . SET verified(regname)=""
	QUIT quit
