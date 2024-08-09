;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2022-2024 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Randomly generate patterns for calls to the DATE_FORMAT SQL function.
octo288() ;
	set pat("a")=""
	set pat("b")=""
	set pat("c")=""
	set pat("D")=""
	set pat("d")=""
	set pat("e")=""
	set pat("f")=""
	set pat("H")=""
	set pat("h")=""
	set pat("I")=""
	set pat("i")=""
	set pat("j")=""
	set pat("k")=""
	set pat("l")=""
	set pat("M")=""
	set pat("m")=""
	set pat("p")=""
	set pat("r")=""
	set pat("S")=""
	set pat("s")=""
	set pat("T")=""
	set pat("U")=""
	set pat("u")=""
	set pat("V")=""
	set pat("v")=""
	set pat("W")=""
	set pat("w")=""
	set pat("X")=""
	set pat("x")=""
	set pat("Y")=""
	set pat("y")=""
	set pat("%")=""
	set subs="" for  set subs=$order(pat(subs))  quit:subs=""  set xref($incr(xref))=subs
	for i=1:1:10 do
	. set year=1899+$random(256)
	. set month=$translate($justify($random(13),2)," ","0")
	. set day=$translate($justify($random(32),2)," ","0")
	. set optionalminus=$select($random(2):"-",1:"")
	. set hour=$translate($justify($random(26),2)," ","0")
	. set minute=$translate($justify($random(61),2)," ","0")
	. set second=$translate($justify($random(61),2)," ","0")
	. set chooseLiteral=0
	. set randInt=$random(2) ; selects whether to choose timestamp or date value
	. ; Following set condition is used to decide whether date/time literal needs to be formed.
	. ; Reason behind the conditions used:
	. ; 1. Date/time type doesn't allow in-exact dates for text format
	. ; 2. Cannot use TIMESTAMP if `year` is not in 1970 to 2038 mysql type range
	. ; 3. To keep things simple day is checked to be less than 28 (Feb) otherwise based on month we need to allow date value
	. ;    which seems like too much complications for this test.
	. ; 4. Avoid using DATE (randInt=0) value (y-m-d) for TIMESTAMP as its not allowed in mysql
	. set:randInt&((+year<2039)&(+minute<60)&(+second<60)&(+hour<24)&((+month>0)&(+month<13))&((+day>0)&(+day<28))) chooseLiteral=$random(2)
	. if randInt do
	. . set delimiter=" "
	. . set:chooseLiteral delimiter=$select($random(2):" ",1:"T"),optionalminus="" ; Optional minus is not allowed in Octo date/time literals
	. . set time=year_"-"_month_"-"_day_delimiter_optionalminus_hour_":"_minute_":"_second
	. else  do
	. . set time=year_"-"_month_"-"_day
	. set xstr="SELECT DATE_FORMAT("
	. set:chooseLiteral xstr=xstr_"TIMESTAMP"
	. set xstr=xstr_"'"_time_"', '"
	. set numpats=1+$random(xref)
	. for j=1:1:numpats do
	. . set randpat=xref(1+$random(xref))
	. . set xstr=xstr_"%"_randpat_" "
	. set xstr=xstr_"');"
	. write xstr,!
	quit
