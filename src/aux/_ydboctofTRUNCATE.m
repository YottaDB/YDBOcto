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

; Implements the SQL TRUNCATE function (truncate to a set number of digits)
; To truncate to the nearest integer, use a precision of 0.
; If `num` has fewer digits than `precision`, it will be zero-padded to `precision`.
; If `precision < 0`, then any fraction will be removed and the last `-precision` digits of the integer will be removed.
; `precision` must be no less than -43.
TRUNCATE(num,precision)
	quit

PostgreSQL(num,precision)
	quit:$ZYISSQLNULL(num) $ZYSQLNULL
	new t  set precision=$fnumber(precision,"",0),t=(num*(10**precision)\1)/(10**precision)
	quit $select(precision<0:t,1:$fnumber(t,"",precision))

MySQL(num,precision)
	quit:$ZYISSQLNULL(num) $ZYSQLNULL
	new t  set precision=$fnumber(precision,"",0),t=(num*(10**precision)\1)/(10**precision)
	quit $select(precision<0:t,(0=+$piece($fnumber(t,"",precision+1),".",2)):t,1:$fnumber(t,"",precision))
