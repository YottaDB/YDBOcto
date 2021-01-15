;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

zintr	; ---------------------------------------------
	; This entryref is invoked by $ZINTERRUPT on receipt of a SIGUSR1 or SIGUSR2 signal.
	; In case of a SIGUSR1, we want to do ZSHOW dump file creation (i.e. $ZJOBEXAM)
	; In case of a SIGUSR2, we want to cancel the currently running query.
	; Note that support for $ZYINTRSIG happened only in r1.31/r1.32 so if r1.30 or lower
	; skip that check and always do query cancelation like was done previously in rocto.
	; This way we avoid a runtime error for an unknown ISV if using an older YottaDB build.
	; ---------------------------------------------
	NEW zyintrsig
	IF $PIECE($ZYRELEASE," ",2)]"r1.30" XECUTE "SET zyintrsig=$ZYINTRSIG"
	ELSE  SET zyintrsig="SIGUSR2"
	IF zyintrsig="SIGUSR1" DO
	.	; If user has defined env var "ydb_zinterrupt" then use it else use $ZJOBEXAM as default $zinterrupt
	.	NEW zinterrupt
	.	SET zinterrupt=$ztrnlnm("ydb_zinterrupt")
	.	IF ""'=zinterrupt DO
	.	.	; Need dotted DO to ensure $TEST value changes here do not affect the following outer-level ELSE
	.	.	XECUTE zinterrupt
	.	ELSE  IF $ZJOBEXAM()
	ELSE  DO
	.	ZGOTO 1:run^%ydboctoCleanup
	QUIT
