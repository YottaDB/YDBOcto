;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; Copyright (c) 2023 YottaDB LLC and/or its subsidiaries.	;
; All rights reserved.						;
;								;
;	This source code contains the intellectual property	;
;	of its copyright holder(s), and is made available	;
;	under a license.  If you do not know the terms of	;
;	the license, please stop and do not read further.	;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; -----------------------------------------------------------------------------------------------------
; This M program is used by the TPF01 subtest to generate various SQL commands and verify that
; "substring()" function implementation in Octo is same as in Postgres.
; -----------------------------------------------------------------------------------------------------

TPF01	;
	for i=1:1:25 do
	. write "select substring("
	. set str=$$randstr,start=$$randpos,end=$$randpos
	. write str
	. write ",",start
	. write:(0=$random(4)) ",",end
	. write ");",!
	quit

randstr()	;
	new rand
	set rand=$random(4)
	quit:(0=rand) "NULL"
	quit "'abcdefghij'"

randpos()	;
	new rand
	set rand=$random(3)
	quit:(0=rand) "NULL"
	quit:(1=rand) $random(12)
	quit -$random(12)

