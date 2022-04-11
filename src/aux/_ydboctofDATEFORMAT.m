;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2022 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implements the MySQL DATE_FORMAT() function, which populates the given
; format string using the provided date string.
DATEFORMAT(date,format)
	quit

; DATE_FORMAT() is a MySQL-only function, so if the emulation is not MySQL,
; we emit an error.
PostgreSQL(date,format)
	set %ydboctoerror("UNKNOWNFUNCTION",1)="DATE_FORMAT"	; pass parameter to `src/ydb_error_check.c`
	set %ydboctoerror("UNKNOWNFUNCTION",2)="2"		; pass parameter to `src/ydb_error_check.c`
	set %ydboctoerror("UNKNOWNFUNCTION",3)="POSTGRES"	; pass parameter to `src/ydb_error_check.c`
	zmessage %ydboctoerror("UNKNOWNFUNCTION")
	quit $ZYSQLNULL

; Populate the given format string using the provided date string.
MySQL(date,format)
	quit $$DATEFORMAT^%ydboctosqlfunctions(date,format)
