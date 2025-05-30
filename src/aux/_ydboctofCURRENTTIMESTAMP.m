;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021-2025 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implements the SQL CURRENT_TIMESTAMP function, which returns the current
; date and time.
CURRENTTIMESTAMP()
	quit

PostgreSQL()
	; Returns the current date and time in `YYYY-MM-DD HH:MM:SS.US-TT` format
	; (where `US` is microseconds and `TT` is the current timezone as a UTC offset).
	; The accuracy of microseconds has the same limitations as in https://docs.yottadb.com/ProgrammersGuide/isv.html#zhorolog.
	new utcoffset  set utcoffset=$piece($zhorolog,",",4)/3600
	new utcabsolute set utcabsolute=$translate(utcoffset,"-","")
	quit $ZDATE($H,"YYYY-MM-DD 24:60:SS")_"."_$piece($zhorolog,",",3)_$select(utcoffset>0:"-",1:"+")_$translate($justify(utcabsolute,2)," ","0")

MySQL()
	; Returns the current date and time in `YYYY-MM-DD HH:MM:SS` format
	new utcoffset  set utcoffset=$piece($zhorolog,",",4)/3600
	quit $ZDATE($H,"YYYY-MM-DD 24:60:SS")
