;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2020-2023 YottaDB LLC and/or its subsidiaries.	;
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
%ydboctoInit(verbosity)	;
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
	SET %ydboctoerror("INVALIDBOOLEANSYNTAX")=$incr(%ydboctoerrcode) ; signaled by `String2Boolean` in `_ydboctoplanhelpers.m`
	; Below two errors are signaled by `IssueInvalidInputSyntaxError` in `_ydboctoplanhelpers.m`
	SET %ydboctoerror("INVALIDINTEGERSYNTAX")=$incr(%ydboctoerrcode)
	SET %ydboctoerror("INVALIDNUMERICSYNTAX")=$incr(%ydboctoerrcode)
	SET %ydboctoerror("INVALIDESCAPEPATTERN")=$incr(%ydboctoerrcode) ; signaled by `regexmatch` in `_ydboctoplanhelpers.m`
	; Below error is signaled by `Cast2NUMERICWithPrecision` in `_ydboctoplanhelpers.m`
	SET %ydboctoerror("NUMERICOVERFLOW")=$incr(%ydboctoerrcode)
	SET %ydboctoerror("VARCHARTOOLONG")=$incr(%ydboctoerrcode) ; signaled by `SizeCheckVARCHAR` in `_ydboctoplanhelpers.m`
	SET %ydboctoerror("DUPLICATEKEYVALUE")=$incr(%ydboctoerrcode) ; signaled by `DuplicateKeyValue` in `_ydboctoplanhelpers.m`
	SET %ydboctoerror("NULLCOLVALUE")=$incr(%ydboctoerrcode) ; signaled by `NullColValue` in `_ydboctoplanhelpers.m`
	; signaled by `MySQL` labels in various `_ydboctof*.m` routines, e.g. `_ydboctofLPAD.m`.
	SET %ydboctoerror("UNKNOWNFUNCTION")=$incr(%ydboctoerrcode)
	SET %ydboctoerror("CHECKCONSTRAINTVIOLATION")=$incr(%ydboctoerrcode)
	SET %ydboctoerror("UNKNOWNTABLE")=$incr(%ydboctoerrcode)
	; Any additions of error codes needs to happen before the following line (%ydboctoerrcodemax)
	; Changes need to also happen in `ydb_error_check.c` and likely in `_ydboctoplanhelpers.m`
	SET %ydboctoerrcodemax=$incr(%ydboctoerrcode)
	; Set $ETRAP for Octo. Note that "ydb_env_set" sets this ISV to a default value that does a WRITE $ZSTATUS.
	; This can be user-unfriendly for Octo since we would see $ZSTATUS show up for non-YDB errors (which Octo simulates above).
	; Hence the need to clear $ETRAP at Octo startup.
	SET $ETRAP=""
	; ------------------------------------------------------------------------------------------------
	; Perform NullSubs check
	; Also issue a warning if "*" namespace maps to the same region that ^%ydbocto* namespace maps to
	;	as this is likely a user misconfiguration issue. But the check for that uses '$VIEW("REGION","^*")'
	;	which was implemented in `r1.30` (production release) and `r1.29` (development release) hence the
	;	`"r1.29"']` check below.
	; ------------------------------------------------------------------------------------------------
	NEW verified,octogbl,reglist,regnum,regname,numregs,quit,starwarningissued,starregname,ydbrel
	SET quit=0,starwarningissued=0,starregname=""
	SET ydbrel=$PIECE($ZYRELEASE," ",2)
	SET:("r1.29"']ydbrel) starregname=$VIEW("REGION","^*")
	FOR octogbl="^%ydboctoxref","^%ydboctoocto","^%ydboctoschema" DO
	. SET reglist=$VIEW("REGION",octogbl),numregs=$LENGTH(reglist,",")
	. FOR regnum=1:1:numregs DO
	. . SET regname=$PIECE(reglist,",",regnum)
	. . QUIT:$DATA(verified(regname))
	. . ; 3=ERROR level from VERBOSITY_LEVEL enum in errors.h. Changes there should also be reflected here.
	. . DO:('starwarningissued&(regname=starregname)&(3>=verbosity))
	. . . WRITE "[ WARN] Global "_octogbl_" maps to default region "_regname_". Recommended mapping for ^%ydbocto* is to a separate region",!
	. . . USE $PRINCIPAL	; In case principal device is terminal, above WRITE is flushed
	. . . SET starwarningissued=1
	. . DO:($$^%PEEKBYNAME("sgmnt_data.null_subs",regname)'=1)
	. . . WRITE "[ERROR] Null subscripts must be enabled for database file ["_$VIEW("GVFILE",regname)_"] : region ["_regname_"] (global "_octogbl_" maps to this region)",!
	. . . USE $PRINCIPAL	; In case principal device is terminal, above WRITE is flushed
	. . . SET quit=1
	. . SET verified(regname)=""
	; $ZTRIGGER invocation in Octo (happens in dollarZTRIGGER^%ydboctoplanhelpers) can cause output like the following.
	;	Added SET and/or Non-SET trigger on ^names named %ydboctoTOMMMsQ8ks3NI1C42wS8
	; But we do not want this output to confuse the Octo user who is expecting some query results.
	; Disable $ZTRIGGER related output using the below VIEW command. But this command only got implemented in YottaDB `r1.32`
	; (and the development release `r1.31`). Hence the check for r1.31 below.
	VIEW:("r1.31"']ydbrel) "ZTRIGGER_OUTPUT":0
	QUIT quit
