;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2019 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -----------------------------------------------------------------------------------------------------
; This program generates random SQL queries to test ANY/SOME/ALL operators.
; And runs the same queries against Postgres and Octo and verifies the outputs are identical.
; Certain features of this test are currently disabled due to pending issues. Those lines are marked with a ###TMPDISABLE.
; -----------------------------------------------------------------------------------------------------

TAO01	;
	; Random choices of quantifiers
	set quantifier=-1
	set quantifier($increment(quantifier))="ANY"
	set quantifier($increment(quantifier))="ALL"
	set quantifier($increment(quantifier))="SOME"
	if $increment(quantifier)
	; Random choice of comparison operators
	set compop=-1
	set compop($increment(compop))="="
	set compop($increment(compop))="!="
	set compop($increment(compop))="<>"
	set compop($increment(compop))="<"
	set compop($increment(compop))=">"
	set compop($increment(compop))="<="
	set compop($increment(compop))=">="
	if $increment(compop)
	; Random choice of arithmetic operators
	set arithop=-1
	set arithop($increment(arithop))="+"
	set arithop($increment(arithop))="-"
	set arithop($increment(arithop))="*"
	; set arithop($increment(arithop))="/"	; ###TMPDISABLE : Uncomment this line once #365 is fixed.
	set arithop($increment(arithop))="%"
	if $increment(arithop)
	;
	set numqueries=20	; generate 20 queries so as not to take a long time for this test to run in pipeline
	for i=1:1:numqueries do
	. set sqlquery="SELECT * FROM names n1 WHERE "_$$boolexpr()_";"
	. set file="TAO01_"_$translate($justify($increment(q),2)," ","0")_".sql"
	. open file:(newversion)  use file
	. write sqlquery,!
	. close file
	quit
	;

boolexpr();
	new boolexpr,i
	set notdepth=$random(3)
	set boolexpr=""
	for i=1:1:notdepth set boolexpr=boolexpr_"NOT("
	set boolexpr=boolexpr_"n1.id "_compop($random(compop))_" "_quantifier($random(quantifier))
	set boolexpr=boolexpr_" (select n2.id from names n2 where n1.id "
	; Do "1+$random(...)" below to avoid modulo with 0 in case it gets randomly chosen
	set boolexpr=boolexpr_compop($random(compop))_" n2.id "_arithop($random(arithop))_" "_(1+$random(6))
	set:'$random(4) boolexpr=boolexpr_" LIMIT "_$random(7)	; randomly add LIMIT
	for i=0:1:notdepth set boolexpr=boolexpr_")"
	quit boolexpr
