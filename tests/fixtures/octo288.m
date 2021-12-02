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
	. if $random(2) do
	. . set time=year_"-"_month_"-"_day_" "_optionalminus_hour_":"_minute_":"_second
	. else  do
	. . set time=year_"-"_month_"-"_day
	. set xstr="SELECT DATE_FORMAT('"_time_"', '"
	. set numpats=1+$random(xref)
	. for j=1:1:numpats do
	. . set randpat=xref(1+$random(xref))
	. . set xstr=xstr_"%"_randpat_" "
	. set xstr=xstr_"');"
	. write xstr,!
	quit
