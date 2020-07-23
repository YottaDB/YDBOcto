;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2020 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -------------------------------------------------------------
; This implements the SQL standard functions (e.g. ABS)
; -------------------------------------------------------------

%ydboctosqlfunctions	;
	QUIT

ABS(num)
	; Implements the SQL ABS function (returns absolute value of an input number)
	quit $select(num<0:-num,1:+num)

ROUND(num,precision)
	; Implements the SQL ROUND function (rounds to a set number of digits)
	; To round to the nearest integer, use a precision of 0.
	; If `precision < 0`, then any fraction will be removed and `num` will be rounded to `10**(-precision)`.
	; If `precision` is a fraction, it will be rounded to the nearest integer.
	; TODO: rounding precision will no longer be necessary when https://gitlab.com/YottaDB/DBMS/YDBOcto/-/issues/565 is fixed
	new pow  set precision=$fnumber(precision,"",0),pow=10**(-precision)
	quit $select(precision<0:$fnumber(num/pow,"",0)*pow,1:$fnumber(num,"",precision))

TRUNC(num,precision)
	; Implements the SQL TRUNCATE function (truncate to a set number of digits)
	; To truncate to the nearest integer, use a precision of 0.
	; If `num` has fewer digits than `precision`, it will be zero-padded to `precision`.
	; If `precision < 0`, then any fraction will be removed and the last `-precision` digits of the integer will be removed.
	; `precision` must be no less than -43.
	new t  set precision=$fnumber(precision,"",0),t=(num*(10**precision)\1)/(10**precision)
	quit $select(precision<0:t,1:$fnumber(t,"",precision))
