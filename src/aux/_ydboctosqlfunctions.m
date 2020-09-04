;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2021 YottaDB LLC and/or its subsidiaries.	;
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

CONCAT(str1,str2,str3)
	; Implements the SQL CONCAT function (concatenates passed strings)
	; Additional checks are used for overloading the SQL function definition.
	new result
	set result=str1
	set:(0'=$DATA(str2)) result=result_str2
	set:(0'=$DATA(str3)) result=result_str3

	quit result

REPLACE(src,from,to) ; TODO this is just a placeholder
	QUIT src

ROUND(num,precision)
	; Implements the SQL ROUND function (rounds to a set number of digits)
	; To round to the nearest integer, use a precision of 0.
	; If `precision < 0`, then any fraction will be removed and `num` will be rounded to `10**(-precision)`.
	; If `precision` is a fraction, it will be rounded to the nearest integer.
	quit:$ZYISSQLNULL(num) $ZYSQLNULL
	new pow  set pow=10**(-precision)
	quit $select(precision<0:$fnumber(num/pow,"",0)*pow,1:$fnumber(num,"",precision))

TRUNC(num,precision)
	; Implements the SQL TRUNCATE function (truncate to a set number of digits)
	; To truncate to the nearest integer, use a precision of 0.
	; If `num` has fewer digits than `precision`, it will be zero-padded to `precision`.
	; If `precision < 0`, then any fraction will be removed and the last `-precision` digits of the integer will be removed.
	; `precision` must be no less than -43.
	quit:$ZYISSQLNULL(num) $ZYSQLNULL
	new t  set precision=$fnumber(precision,"",0),t=(num*(10**precision)\1)/(10**precision)
	quit $select(precision<0:t,1:$fnumber(t,"",precision))

VERSION()
	quit "PostgreSQL 9.6.5 on x86_64-pc-linux-gnu, compiled by gcc (GCC) 7.1.1 20170630, 64-bit"
