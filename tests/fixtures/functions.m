;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2021-2024 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Helper functions used by bats tests

samevalue(param)	; Returns input parameter as output (used by a few bats tests)
	quit param

; Function receives input in mdy form and returns in ymd form
samevaluemdy(param)
	quit:$ZYISSQLNULL(param) param
	new val
	set val=param
	set month=$piece(val,"-",1)
	set day=$piece(val,"-",2)
	set year=$piece(val,"-",3)
	set rest=$piece(year," ",2)
	set year=$piece(year," ",1)
	quit year_"-"_month_"-"_day_$select(rest'="":" "_rest,1:"")

threeparmfunc(parm1,parm2,parm3)	; Returns first input parmeter as output
	quit parm1
