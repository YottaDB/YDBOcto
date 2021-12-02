;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021-2022 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

type1()
	; Returns the current time in `HH:MM:SS.US-TT` format
	quit $$type1^%ydboctofCURRENTTIME()

type2()
	; Returns the current time in `YYYY-MM-DD HH:MM:SS` format
	quit $$type2^%ydboctofCURRENTTIMESTAMP()

PostgreSQL()
	quit $$type1()

MySQL()
	quit $$type2()
