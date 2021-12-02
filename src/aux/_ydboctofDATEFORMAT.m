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

type1(date,format)
	set %ydboctoerror("UNKNOWNFUNCTION",1)="DATE_FORMAT"	; pass parameter to `src/ydb_error_check.c`
	set %ydboctoerror("UNKNOWNFUNCTION",2)="2"	; pass parameter to `src/ydb_error_check.c`
	set %ydboctoerror("UNKNOWNFUNCTION",3)="POSTGRES"	; pass parameter to `src/ydb_error_check.c`
	zmessage %ydboctoerror("UNKNOWNFUNCTION")
	quit $ZYSQLNULL

type2(date,format)
	quit $$DATEFORMAT^%ydboctosqlfunctions(date,format)

PostgreSQL(date,format)
	quit $$type1(date,format)

MySQL(date,format)
	quit $$type2(date,format)
