;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2024 YottaDB LLC and/or its subsidiaries.	;
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

; Empty string can be passed as any argument
;
REPLACE(src,from,to)
	; If `src` is NULL return NULL
	QUIT:$ZYISSQLNULL(src) $ZYSQLNULL
	; If `from` is NULL treat it as empty string
	SET:$ZYISSQLNULL(from) from=""
	; If `to` is NULL treat it as empty string
	SET:$ZYISSQLNULL(to) to=""
	; length of src
	SET len=$length(src)
	; length of from
	SET n=$length(from)
	; j is used to iterate `from`
	SET j=1
	; i is used to iterate `src`
	SET i=1
	; res holds the final value to be returned
	SET res=""
	for  do  QUIT:(i>len)
	. set ch=$extract(src,i)
	. if (ch=$extract(from,j)) do
	. . if n=1 set res=res_to,i=i+1 QUIT
	. . set end=0,k=1 ; k is used to iterate over `from`, `end` is set where there is no match to exit loop
	. . set kstr=ch
	. . for  do  QUIT:(end=1)!(k=n)
	. . . set ich=$extract(src,i+k)
	. . . if '(ich=$extract(from,j+k)) set end=1
	. . . else  set k=k+1,kstr=kstr_ich
	. . if (1'=end) set res=res_to,i=i+n ; for iterate over entire `from` so replace it
	. . else  set res=res_kstr,i=i+k
	. else  set res=res_ch,i=i+1 ; no matching char, copy current value to `res`
	QUIT res

ROUND(num,precision)
	; Implements the SQL ROUND function (rounds to a set number of digits)
	; To round to the nearest integer, use a precision of 0.
	; If `precision < 0`, then any fraction will be removed and `num` will be rounded to `10**(-precision)`.
	; If `precision` is a fraction, it will be rounded to the nearest integer.
	quit:$ZYISSQLNULL(num) $ZYSQLNULL
	new pow  set pow=10**(-precision)
	quit $select(precision<0:$fnumber(num/pow,"",0)*pow,1:$fnumber(num,"",precision))

VERSION()
	quit "PostgreSQL 13.0.0 on x86_64-pc-linux-gnu, compiled by gcc (GCC) 7.1.1 20170630, 64-bit"

DAY(date)
	quit:$ZYISSQLNULL(date) $ZYSQLNULL
	quit $$DATEFORMAT^%ydboctosqlfunctions(date,"%e")

DATEFORMAT(date,format)
	new result
	quit:$ZYISSQLNULL(date)!$ZYISSQLNULL(date) $ZYSQLNULL
	set result=$&octo.ydboctoDateFormatM(date,format)
	set:""=result result=$ZYSQLNULL
	quit result
