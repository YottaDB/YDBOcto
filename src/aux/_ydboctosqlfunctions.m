;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019-2026 YottaDB LLC and/or its subsidiaries.	;
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

MOD(a,b)
	; Implements the SQL standard MOD function (YDBOcto#1030): performs the same de facto standard modulo
	; operation (truncated-division remainder, sign of the dividend) as the "a % b" operator.
	quit $$Modulo^%ydboctoplanhelpers(a,b)

; Empty string can be passed as any argument
;
REPLACE(src,from,to)
	; If `src` is NULL return NULL
	QUIT:$ZYISSQLNULL(src) $ZYSQLNULL
	NEW i,j,k,len,n,res,ch,end,kstr,ich
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
	; Implements the SQL day(VARCHAR)/dayofmonth(VARCHAR) functions: returns the day of the month
	; (e.g. "15", no leading zero) from a "YYYY-MM-DD" string, optionally followed by a time component
	; (which is ignored). Returns $ZYSQLNULL for NULL input, a malformed date string, or a day that is
	; out of range for its month (e.g. day 30 of February). A day field of 0 (e.g. "1999-06-00") returns
	; "0", matching MySQL's DAYOFMONTH() -- except an all-zero date ("0000-00-00"), which returns NULL.
	new year,month,day,leap,daysinmonth
	quit:$ZYISSQLNULL(date) $ZYSQLNULL
	set year=$piece(date,"-",1),month=$piece(date,"-",2),day=$piece($piece(date,"-",3)," ",1)
	quit:'(year?1.4N)!'(month?1.2N)!'(day?1.2N) $ZYSQLNULL
	set year=+year,month=+month,day=+day
	quit:(month>12)!(day>31) $ZYSQLNULL
	quit:(year=0)&(month=0)&(day=0) $ZYSQLNULL
	quit:day=0 "0"
	; A month of 0 is tolerated (as if it were a 31-day month), matching MySQL's leniency here.
	set leap=((year#4)=0)&(((year#100)'=0)!((year#400)=0))
	set daysinmonth=$select((month=4)!(month=6)!(month=9)!(month=11):30,month=2:$select(leap:29,1:28),1:31)
	quit:day>daysinmonth $ZYSQLNULL
	quit day

DAYFROMDATE(date)
	; Implements the SQL day(DATE)/dayofmonth(DATE) functions. Octo's DATE type guarantees `date` is
	; already a valid, canonical "YYYY-MM-DD" string regardless of the column's internal storage format
	; (verified empirically across text/horolog/fileman storage), so no parsing or validation is needed
	; here -- just extract the day field and strip any leading zero.
	quit:$ZYISSQLNULL(date) $ZYSQLNULL
	quit +$piece(date,"-",3)
