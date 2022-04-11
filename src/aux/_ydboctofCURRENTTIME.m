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

; Implements the SQL CURRENT_TIME function, which returns the current
; time.
CURRENTTIME()
	quit

PostgreSQL()
	; Returns the current time in `HH:MM:SS.US-TT` format
	quit $extract($$PostgreSQL^%ydboctofCURRENTTIMESTAMP(),12,29)

MySQL()
	; Returns the current time in `HH:MM:SS` format
	quit $extract($$MySQL^%ydboctofCURRENTTIMESTAMP(),12,21)
